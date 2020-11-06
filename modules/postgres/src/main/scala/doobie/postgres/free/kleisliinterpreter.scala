// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.free

// Library imports
import cats.~>
import cats.data.Kleisli
import cats.effect.{ Async, Blocker, ContextShift, ExitCase }
import cats.implicits._
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import com.github.ghik.silencer.silent
import io.chrisdavenport.log4cats.MessageLogger

// Types referenced in the JDBC API
import java.io.InputStream
import java.io.OutputStream
import java.io.Reader
import java.io.Writer
import java.lang.Class
import java.lang.String
import java.sql.ResultSet
import java.sql.{ Array => SqlArray }
import java.util.Map
import org.postgresql.PGConnection
import org.postgresql.PGNotification
import org.postgresql.copy.{ CopyDual => PGCopyDual }
import org.postgresql.copy.{ CopyIn => PGCopyIn }
import org.postgresql.copy.{ CopyManager => PGCopyManager }
import org.postgresql.copy.{ CopyOut => PGCopyOut }
import org.postgresql.fastpath.FastpathArg
import org.postgresql.fastpath.{ Fastpath => PGFastpath }
import org.postgresql.jdbc.AutoSave
import org.postgresql.jdbc.PreferQueryMode
import org.postgresql.largeobject.LargeObject
import org.postgresql.largeobject.LargeObjectManager
import org.postgresql.replication.PGReplicationConnection
import org.postgresql.util.ByteStreamWriter

// Algebras and free monads thereof referenced by our interpreter.
import doobie.postgres.free.copyin.{ CopyInIO, CopyInOp }
import doobie.postgres.free.copymanager.{ CopyManagerIO, CopyManagerOp }
import doobie.postgres.free.copyout.{ CopyOutIO, CopyOutOp }
import doobie.postgres.free.fastpath.{ FastpathIO, FastpathOp }
import doobie.postgres.free.largeobject.{ LargeObjectIO, LargeObjectOp }
import doobie.postgres.free.largeobjectmanager.{ LargeObjectManagerIO, LargeObjectManagerOp }
import doobie.postgres.free.pgconnection.{ PGConnectionIO, PGConnectionOp }

object KleisliInterpreter {

  def apply[M[_]](b: Blocker)(
    implicit am: Async[M],
             cs: ContextShift[M],
             lo: MessageLogger[M],
  ): KleisliInterpreter[M] =
    new KleisliInterpreter[M] {
      val asyncM = am
      val loggerM = lo
      val contextShiftM = cs
      val blocker = b
    }

}

// Family of interpreters into Kleisli arrows for some monad M.
@silent("deprecated")
trait KleisliInterpreter[M[_]] { outer =>

  implicit val asyncM: Async[M]
  implicit val loggerM: MessageLogger[M]

  // We need these things in order to provide ContextShift[ConnectionIO] and so on, and also
  // to support shifting blocking operations to another pool.
  val contextShiftM: ContextShift[M]
  val blocker: Blocker

  // The 7 interpreters, with definitions below. These can be overridden to customize behavior.
  lazy val CopyInInterpreter: CopyInOp ~> Kleisli[M, PGCopyIn, *] = new CopyInInterpreter { }
  lazy val CopyManagerInterpreter: CopyManagerOp ~> Kleisli[M, PGCopyManager, *] = new CopyManagerInterpreter { }
  lazy val CopyOutInterpreter: CopyOutOp ~> Kleisli[M, PGCopyOut, *] = new CopyOutInterpreter { }
  lazy val FastpathInterpreter: FastpathOp ~> Kleisli[M, PGFastpath, *] = new FastpathInterpreter { }
  lazy val LargeObjectInterpreter: LargeObjectOp ~> Kleisli[M, LargeObject, *] = new LargeObjectInterpreter { }
  lazy val LargeObjectManagerInterpreter: LargeObjectManagerOp ~> Kleisli[M, LargeObjectManager, *] = new LargeObjectManagerInterpreter { }
  lazy val PGConnectionInterpreter: PGConnectionOp ~> Kleisli[M, PGConnection, *] = new PGConnectionInterpreter { }

  // Some methods are common to all interpreters and can be overridden to change behavior globally.
  def primitive[J, A](f: J => A, operation: => String, args: Any*): Kleisli[M, J, A] = Kleisli { a =>
    // primitive JDBC methods throw exceptions and so do we when reading values
    // so catch any non-fatal exceptions and lift them into the effect
    blocker.blockOn[M, A] {
      import scala.Predef._

      def logMessage: String =
        s"${a.getClass.getSimpleName}@${Integer.toHexString(System.identityHashCode(a))} ${operation}${if (args.isEmpty) "" else args.mkString("(", ",", ")")}"

      (asyncM.delay(f(a)) <* loggerM.trace(logMessage)).onError {
        case NonFatal(e) => loggerM.error(doobie.free.ErrorFormatter.format(a, operation, args, e))
      }

    } (contextShiftM)
  }
  def delay[J, A](a: () => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.delay(a()))
  def raw[J, A](message: => String, f: J => A): Kleisli[M, J, A] = primitive(f, message)
  def raiseError[J, A](e: Throwable): Kleisli[M, J, A] = Kleisli(_ => asyncM.raiseError(e))
  def async[J, A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, J, A] = Kleisli(_ => asyncM.async(k))
  def embed[J, A](e: Embedded[A]): Kleisli[M, J, A] =
    e match {
      case Embedded.CopyIn(j, fa) => Kleisli(_ => fa.foldMap(CopyInInterpreter).run(j))
      case Embedded.CopyManager(j, fa) => Kleisli(_ => fa.foldMap(CopyManagerInterpreter).run(j))
      case Embedded.CopyOut(j, fa) => Kleisli(_ => fa.foldMap(CopyOutInterpreter).run(j))
      case Embedded.Fastpath(j, fa) => Kleisli(_ => fa.foldMap(FastpathInterpreter).run(j))
      case Embedded.LargeObject(j, fa) => Kleisli(_ => fa.foldMap(LargeObjectInterpreter).run(j))
      case Embedded.LargeObjectManager(j, fa) => Kleisli(_ => fa.foldMap(LargeObjectManagerInterpreter).run(j))
      case Embedded.PGConnection(j, fa) => Kleisli(_ => fa.foldMap(PGConnectionInterpreter).run(j))
    }

  // Logger
  def error[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.error(message))
  def warn[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.warn(message))
  def info[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.info(message))
  def debug[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.debug(message))
  def trace[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.trace(message))

  // Interpreters
  trait CopyInInterpreter extends CopyInOp.Visitor[Kleisli[M, PGCopyIn, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](message: => String, f: PGCopyIn => A): Kleisli[M, PGCopyIn, A] = outer.raw(message, f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGCopyIn, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, PGCopyIn, A] = outer.delay(a)
    override def raiseError[A](err: Throwable): Kleisli[M, PGCopyIn, A] = outer.raiseError(err)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, PGCopyIn, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyInIO[Unit]): Kleisli[M, PGCopyIn, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: CopyInIO[A], f: Throwable => CopyInIO[A]): Kleisli[M, PGCopyIn, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: CopyInIO[A])(use: A => CopyInIO[B])(release: (A, ExitCase[Throwable]) => CopyInIO[Unit]): Kleisli[M, PGCopyIn, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, PGCopyIn, Unit] =
      Kleisli(_ => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: CopyInIO[A]): Kleisli[M, PGCopyIn, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // Logger
    def error(message: => String): Kleisli[M, PGCopyIn, Unit] = outer.error(message)
    def warn(message: => String): Kleisli[M, PGCopyIn, Unit] = outer.warn(message)
    def info(message: => String): Kleisli[M, PGCopyIn, Unit] = outer.info(message)
    def debug(message: => String): Kleisli[M, PGCopyIn, Unit] = outer.debug(message)
    def trace(message: => String): Kleisli[M, PGCopyIn, Unit] = outer.trace(message)

    // domain-specific operations are implemented in terms of `primitive`
    override def cancelCopy = primitive(_.cancelCopy, "cancelCopy")
    override def endCopy = primitive(_.endCopy, "endCopy")
    override def flushCopy = primitive(_.flushCopy, "flushCopy")
    override def getFieldCount = primitive(_.getFieldCount, "getFieldCount")
    override def getFieldFormat(a: Int) = primitive(_.getFieldFormat(a), "getFieldFormat", a)
    override def getFormat = primitive(_.getFormat, "getFormat")
    override def getHandledRowCount = primitive(_.getHandledRowCount, "getHandledRowCount")
    override def isActive = primitive(_.isActive, "isActive")
    override def writeToCopy(a: Array[Byte], b: Int, c: Int) = primitive(_.writeToCopy(a, b, c), "writeToCopy", a, b, c)
    override def writeToCopy(a: ByteStreamWriter) = primitive(_.writeToCopy(a), "writeToCopy", a)

  }

  trait CopyManagerInterpreter extends CopyManagerOp.Visitor[Kleisli[M, PGCopyManager, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](message: => String, f: PGCopyManager => A): Kleisli[M, PGCopyManager, A] = outer.raw(message, f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGCopyManager, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, PGCopyManager, A] = outer.delay(a)
    override def raiseError[A](err: Throwable): Kleisli[M, PGCopyManager, A] = outer.raiseError(err)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, PGCopyManager, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyManagerIO[Unit]): Kleisli[M, PGCopyManager, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: CopyManagerIO[A], f: Throwable => CopyManagerIO[A]): Kleisli[M, PGCopyManager, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: CopyManagerIO[A])(use: A => CopyManagerIO[B])(release: (A, ExitCase[Throwable]) => CopyManagerIO[Unit]): Kleisli[M, PGCopyManager, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, PGCopyManager, Unit] =
      Kleisli(_ => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: CopyManagerIO[A]): Kleisli[M, PGCopyManager, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // Logger
    def error(message: => String): Kleisli[M, PGCopyManager, Unit] = outer.error(message)
    def warn(message: => String): Kleisli[M, PGCopyManager, Unit] = outer.warn(message)
    def info(message: => String): Kleisli[M, PGCopyManager, Unit] = outer.info(message)
    def debug(message: => String): Kleisli[M, PGCopyManager, Unit] = outer.debug(message)
    def trace(message: => String): Kleisli[M, PGCopyManager, Unit] = outer.trace(message)

    // domain-specific operations are implemented in terms of `primitive`
    override def copyDual(a: String) = primitive(_.copyDual(a), "copyDual", a)
    override def copyIn(a: String) = primitive(_.copyIn(a), "copyIn", a)
    override def copyIn(a: String, b: ByteStreamWriter) = primitive(_.copyIn(a, b), "copyIn", a, b)
    override def copyIn(a: String, b: InputStream) = primitive(_.copyIn(a, b), "copyIn", a, b)
    override def copyIn(a: String, b: InputStream, c: Int) = primitive(_.copyIn(a, b, c), "copyIn", a, b, c)
    override def copyIn(a: String, b: Reader) = primitive(_.copyIn(a, b), "copyIn", a, b)
    override def copyIn(a: String, b: Reader, c: Int) = primitive(_.copyIn(a, b, c), "copyIn", a, b, c)
    override def copyOut(a: String) = primitive(_.copyOut(a), "copyOut", a)
    override def copyOut(a: String, b: OutputStream) = primitive(_.copyOut(a, b), "copyOut", a, b)
    override def copyOut(a: String, b: Writer) = primitive(_.copyOut(a, b), "copyOut", a, b)

  }

  trait CopyOutInterpreter extends CopyOutOp.Visitor[Kleisli[M, PGCopyOut, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](message: => String, f: PGCopyOut => A): Kleisli[M, PGCopyOut, A] = outer.raw(message, f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGCopyOut, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, PGCopyOut, A] = outer.delay(a)
    override def raiseError[A](err: Throwable): Kleisli[M, PGCopyOut, A] = outer.raiseError(err)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, PGCopyOut, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyOutIO[Unit]): Kleisli[M, PGCopyOut, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: CopyOutIO[A], f: Throwable => CopyOutIO[A]): Kleisli[M, PGCopyOut, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: CopyOutIO[A])(use: A => CopyOutIO[B])(release: (A, ExitCase[Throwable]) => CopyOutIO[Unit]): Kleisli[M, PGCopyOut, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, PGCopyOut, Unit] =
      Kleisli(_ => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: CopyOutIO[A]): Kleisli[M, PGCopyOut, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // Logger
    def error(message: => String): Kleisli[M, PGCopyOut, Unit] = outer.error(message)
    def warn(message: => String): Kleisli[M, PGCopyOut, Unit] = outer.warn(message)
    def info(message: => String): Kleisli[M, PGCopyOut, Unit] = outer.info(message)
    def debug(message: => String): Kleisli[M, PGCopyOut, Unit] = outer.debug(message)
    def trace(message: => String): Kleisli[M, PGCopyOut, Unit] = outer.trace(message)

    // domain-specific operations are implemented in terms of `primitive`
    override def cancelCopy = primitive(_.cancelCopy, "cancelCopy")
    override def getFieldCount = primitive(_.getFieldCount, "getFieldCount")
    override def getFieldFormat(a: Int) = primitive(_.getFieldFormat(a), "getFieldFormat", a)
    override def getFormat = primitive(_.getFormat, "getFormat")
    override def getHandledRowCount = primitive(_.getHandledRowCount, "getHandledRowCount")
    override def isActive = primitive(_.isActive, "isActive")
    override def readFromCopy = primitive(_.readFromCopy, "readFromCopy")
    override def readFromCopy(a: Boolean) = primitive(_.readFromCopy(a), "readFromCopy", a)

  }

  trait FastpathInterpreter extends FastpathOp.Visitor[Kleisli[M, PGFastpath, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](message: => String, f: PGFastpath => A): Kleisli[M, PGFastpath, A] = outer.raw(message, f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGFastpath, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, PGFastpath, A] = outer.delay(a)
    override def raiseError[A](err: Throwable): Kleisli[M, PGFastpath, A] = outer.raiseError(err)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, PGFastpath, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => FastpathIO[Unit]): Kleisli[M, PGFastpath, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: FastpathIO[A], f: Throwable => FastpathIO[A]): Kleisli[M, PGFastpath, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: FastpathIO[A])(use: A => FastpathIO[B])(release: (A, ExitCase[Throwable]) => FastpathIO[Unit]): Kleisli[M, PGFastpath, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, PGFastpath, Unit] =
      Kleisli(_ => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: FastpathIO[A]): Kleisli[M, PGFastpath, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // Logger
    def error(message: => String): Kleisli[M, PGFastpath, Unit] = outer.error(message)
    def warn(message: => String): Kleisli[M, PGFastpath, Unit] = outer.warn(message)
    def info(message: => String): Kleisli[M, PGFastpath, Unit] = outer.info(message)
    def debug(message: => String): Kleisli[M, PGFastpath, Unit] = outer.debug(message)
    def trace(message: => String): Kleisli[M, PGFastpath, Unit] = outer.trace(message)

    // domain-specific operations are implemented in terms of `primitive`
    override def addFunction(a: String, b: Int) = primitive(_.addFunction(a, b), "addFunction", a, b)
    override def addFunctions(a: ResultSet) = primitive(_.addFunctions(a), "addFunctions", a)
    override def fastpath(a: Int, b: Array[FastpathArg]) = primitive(_.fastpath(a, b), "fastpath", a, b)
    override def fastpath(a: Int, b: Boolean, c: Array[FastpathArg]) = primitive(_.fastpath(a, b, c), "fastpath", a, b, c)
    override def fastpath(a: String, b: Array[FastpathArg]) = primitive(_.fastpath(a, b), "fastpath", a, b)
    override def fastpath(a: String, b: Boolean, c: Array[FastpathArg]) = primitive(_.fastpath(a, b, c), "fastpath", a, b, c)
    override def getData(a: String, b: Array[FastpathArg]) = primitive(_.getData(a, b), "getData", a, b)
    override def getID(a: String) = primitive(_.getID(a), "getID", a)
    override def getInteger(a: String, b: Array[FastpathArg]) = primitive(_.getInteger(a, b), "getInteger", a, b)
    override def getLong(a: String, b: Array[FastpathArg]) = primitive(_.getLong(a, b), "getLong", a, b)
    override def getOID(a: String, b: Array[FastpathArg]) = primitive(_.getOID(a, b), "getOID", a, b)

  }

  trait LargeObjectInterpreter extends LargeObjectOp.Visitor[Kleisli[M, LargeObject, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](message: => String, f: LargeObject => A): Kleisli[M, LargeObject, A] = outer.raw(message, f)
    override def embed[A](e: Embedded[A]): Kleisli[M, LargeObject, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, LargeObject, A] = outer.delay(a)
    override def raiseError[A](err: Throwable): Kleisli[M, LargeObject, A] = outer.raiseError(err)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, LargeObject, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => LargeObjectIO[Unit]): Kleisli[M, LargeObject, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: LargeObjectIO[A], f: Throwable => LargeObjectIO[A]): Kleisli[M, LargeObject, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: LargeObjectIO[A])(use: A => LargeObjectIO[B])(release: (A, ExitCase[Throwable]) => LargeObjectIO[Unit]): Kleisli[M, LargeObject, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, LargeObject, Unit] =
      Kleisli(_ => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: LargeObjectIO[A]): Kleisli[M, LargeObject, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // Logger
    def error(message: => String): Kleisli[M, LargeObject, Unit] = outer.error(message)
    def warn(message: => String): Kleisli[M, LargeObject, Unit] = outer.warn(message)
    def info(message: => String): Kleisli[M, LargeObject, Unit] = outer.info(message)
    def debug(message: => String): Kleisli[M, LargeObject, Unit] = outer.debug(message)
    def trace(message: => String): Kleisli[M, LargeObject, Unit] = outer.trace(message)

    // domain-specific operations are implemented in terms of `primitive`
    override def close = primitive(_.close, "close")
    override def copy = primitive(_.copy, "copy")
    override def getInputStream = primitive(_.getInputStream, "getInputStream")
    override def getInputStream(a: Long) = primitive(_.getInputStream(a), "getInputStream", a)
    override def getLongOID = primitive(_.getLongOID, "getLongOID")
    override def getOID = primitive(_.getOID, "getOID")
    override def getOutputStream = primitive(_.getOutputStream, "getOutputStream")
    override def read(a: Array[Byte], b: Int, c: Int) = primitive(_.read(a, b, c), "read", a, b, c)
    override def read(a: Int) = primitive(_.read(a), "read", a)
    override def seek(a: Int) = primitive(_.seek(a), "seek", a)
    override def seek(a: Int, b: Int) = primitive(_.seek(a, b), "seek", a, b)
    override def seek64(a: Long, b: Int) = primitive(_.seek64(a, b), "seek64", a, b)
    override def size = primitive(_.size, "size")
    override def size64 = primitive(_.size64, "size64")
    override def tell = primitive(_.tell, "tell")
    override def tell64 = primitive(_.tell64, "tell64")
    override def truncate(a: Int) = primitive(_.truncate(a), "truncate", a)
    override def truncate64(a: Long) = primitive(_.truncate64(a), "truncate64", a)
    override def write(a: Array[Byte]) = primitive(_.write(a), "write", a)
    override def write(a: Array[Byte], b: Int, c: Int) = primitive(_.write(a, b, c), "write", a, b, c)

  }

  trait LargeObjectManagerInterpreter extends LargeObjectManagerOp.Visitor[Kleisli[M, LargeObjectManager, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](message: => String, f: LargeObjectManager => A): Kleisli[M, LargeObjectManager, A] = outer.raw(message, f)
    override def embed[A](e: Embedded[A]): Kleisli[M, LargeObjectManager, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, LargeObjectManager, A] = outer.delay(a)
    override def raiseError[A](err: Throwable): Kleisli[M, LargeObjectManager, A] = outer.raiseError(err)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, LargeObjectManager, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => LargeObjectManagerIO[Unit]): Kleisli[M, LargeObjectManager, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: LargeObjectManagerIO[A], f: Throwable => LargeObjectManagerIO[A]): Kleisli[M, LargeObjectManager, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: LargeObjectManagerIO[A])(use: A => LargeObjectManagerIO[B])(release: (A, ExitCase[Throwable]) => LargeObjectManagerIO[Unit]): Kleisli[M, LargeObjectManager, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, LargeObjectManager, Unit] =
      Kleisli(_ => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: LargeObjectManagerIO[A]): Kleisli[M, LargeObjectManager, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // Logger
    def error(message: => String): Kleisli[M, LargeObjectManager, Unit] = outer.error(message)
    def warn(message: => String): Kleisli[M, LargeObjectManager, Unit] = outer.warn(message)
    def info(message: => String): Kleisli[M, LargeObjectManager, Unit] = outer.info(message)
    def debug(message: => String): Kleisli[M, LargeObjectManager, Unit] = outer.debug(message)
    def trace(message: => String): Kleisli[M, LargeObjectManager, Unit] = outer.trace(message)

    // domain-specific operations are implemented in terms of `primitive`
    override def create = primitive(_.create, "create")
    override def create(a: Int) = primitive(_.create(a), "create", a)
    override def createLO = primitive(_.createLO, "createLO")
    override def createLO(a: Int) = primitive(_.createLO(a), "createLO", a)
    override def delete(a: Int) = primitive(_.delete(a), "delete", a)
    override def delete(a: Long) = primitive(_.delete(a), "delete", a)
    override def open(a: Int) = primitive(_.open(a), "open", a)
    override def open(a: Int, b: Boolean) = primitive(_.open(a, b), "open", a, b)
    override def open(a: Int, b: Int) = primitive(_.open(a, b), "open", a, b)
    override def open(a: Int, b: Int, c: Boolean) = primitive(_.open(a, b, c), "open", a, b, c)
    override def open(a: Long) = primitive(_.open(a), "open", a)
    override def open(a: Long, b: Boolean) = primitive(_.open(a, b), "open", a, b)
    override def open(a: Long, b: Int) = primitive(_.open(a, b), "open", a, b)
    override def open(a: Long, b: Int, c: Boolean) = primitive(_.open(a, b, c), "open", a, b, c)
    override def unlink(a: Int) = primitive(_.unlink(a), "unlink", a)
    override def unlink(a: Long) = primitive(_.unlink(a), "unlink", a)

  }

  trait PGConnectionInterpreter extends PGConnectionOp.Visitor[Kleisli[M, PGConnection, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](message: => String, f: PGConnection => A): Kleisli[M, PGConnection, A] = outer.raw(message, f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGConnection, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, PGConnection, A] = outer.delay(a)
    override def raiseError[A](err: Throwable): Kleisli[M, PGConnection, A] = outer.raiseError(err)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, PGConnection, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => PGConnectionIO[Unit]): Kleisli[M, PGConnection, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: PGConnectionIO[A], f: Throwable => PGConnectionIO[A]): Kleisli[M, PGConnection, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: PGConnectionIO[A])(use: A => PGConnectionIO[B])(release: (A, ExitCase[Throwable]) => PGConnectionIO[Unit]): Kleisli[M, PGConnection, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, PGConnection, Unit] =
      Kleisli(_ => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: PGConnectionIO[A]): Kleisli[M, PGConnection, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // Logger
    def error(message: => String): Kleisli[M, PGConnection, Unit] = outer.error(message)
    def warn(message: => String): Kleisli[M, PGConnection, Unit] = outer.warn(message)
    def info(message: => String): Kleisli[M, PGConnection, Unit] = outer.info(message)
    def debug(message: => String): Kleisli[M, PGConnection, Unit] = outer.debug(message)
    def trace(message: => String): Kleisli[M, PGConnection, Unit] = outer.trace(message)

    // domain-specific operations are implemented in terms of `primitive`
    override def addDataType(a: String, b: Class[_ <: org.postgresql.util.PGobject]) = primitive(_.addDataType(a, b), "addDataType", a, b)
    override def addDataType(a: String, b: String) = primitive(_.addDataType(a, b), "addDataType", a, b)
    override def cancelQuery = primitive(_.cancelQuery, "cancelQuery")
    override def createArrayOf(a: String, b: AnyRef) = primitive(_.createArrayOf(a, b), "createArrayOf", a, b)
    override def escapeIdentifier(a: String) = primitive(_.escapeIdentifier(a), "escapeIdentifier", a)
    override def escapeLiteral(a: String) = primitive(_.escapeLiteral(a), "escapeLiteral", a)
    override def getAutosave = primitive(_.getAutosave, "getAutosave")
    override def getBackendPID = primitive(_.getBackendPID, "getBackendPID")
    override def getCopyAPI = primitive(_.getCopyAPI, "getCopyAPI")
    override def getDefaultFetchSize = primitive(_.getDefaultFetchSize, "getDefaultFetchSize")
    override def getFastpathAPI = primitive(_.getFastpathAPI, "getFastpathAPI")
    override def getLargeObjectAPI = primitive(_.getLargeObjectAPI, "getLargeObjectAPI")
    override def getNotifications = primitive(_.getNotifications, "getNotifications")
    override def getNotifications(a: Int) = primitive(_.getNotifications(a), "getNotifications", a)
    override def getParameterStatus(a: String) = primitive(_.getParameterStatus(a), "getParameterStatus", a)
    override def getParameterStatuses = primitive(_.getParameterStatuses, "getParameterStatuses")
    override def getPreferQueryMode = primitive(_.getPreferQueryMode, "getPreferQueryMode")
    override def getPrepareThreshold = primitive(_.getPrepareThreshold, "getPrepareThreshold")
    override def getReplicationAPI = primitive(_.getReplicationAPI, "getReplicationAPI")
    override def setAutosave(a: AutoSave) = primitive(_.setAutosave(a), "setAutosave", a)
    override def setDefaultFetchSize(a: Int) = primitive(_.setDefaultFetchSize(a), "setDefaultFetchSize", a)
    override def setPrepareThreshold(a: Int) = primitive(_.setPrepareThreshold(a), "setPrepareThreshold", a)

  }


}


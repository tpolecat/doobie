// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.postgres.free

// Library imports
import cats.~>
import cats.data.Kleisli
import cats.effect.{ Async, ContextShift, ExitCase }
import scala.concurrent.ExecutionContext

// Types referenced in the JDBC API
import java.io.InputStream
import java.io.OutputStream
import java.io.Reader
import java.io.Writer
import java.lang.Class
import java.lang.String
import java.sql.ResultSet
import org.postgresql.PGConnection
import org.postgresql.copy.{ CopyIn => PGCopyIn }
import org.postgresql.copy.{ CopyManager => PGCopyManager }
import org.postgresql.copy.{ CopyOut => PGCopyOut }
import org.postgresql.fastpath.FastpathArg
import org.postgresql.fastpath.{ Fastpath => PGFastpath }
import org.postgresql.jdbc.AutoSave
import org.postgresql.largeobject.LargeObject
import org.postgresql.largeobject.LargeObjectManager

// Algebras and free monads thereof referenced by our interpreter.
import doobie.postgres.free.copyin.{ CopyInIO, CopyInOp }
import doobie.postgres.free.copymanager.{ CopyManagerIO, CopyManagerOp }
import doobie.postgres.free.copyout.{ CopyOutIO, CopyOutOp }
import doobie.postgres.free.fastpath.{ FastpathIO, FastpathOp }
import doobie.postgres.free.largeobject.{ LargeObjectIO, LargeObjectOp }
import doobie.postgres.free.largeobjectmanager.{ LargeObjectManagerIO, LargeObjectManagerOp }
import doobie.postgres.free.pgconnection.{ PGConnectionIO, PGConnectionOp }

object KleisliInterpreter {

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def apply[M[_]](
    implicit am: Async[M],
             cs: ContextShift[M]
  ): KleisliInterpreter[M] =
    new KleisliInterpreter[M] {
      val asyncM = am
      val contextShiftM = cs
    }

}

// Family of interpreters into Kleisli arrows for some monad M.
@com.github.ghik.silencer.silent // deprecations, unused variables, etc.
trait KleisliInterpreter[M[_]] { outer =>

  implicit val asyncM: Async[M]

  // We need these things in order to provide ContextShift[ConnectionIO] and so on, and also
  // to support shifting blocking operations to another pool.
  val contextShiftM: ContextShift[M]

  // The 7 interpreters, with definitions below. These can be overridden to customize behavior.
  lazy val CopyInInterpreter: CopyInOp ~> Kleisli[M, Env[PGCopyIn], ?] = new CopyInInterpreter { }
  lazy val CopyManagerInterpreter: CopyManagerOp ~> Kleisli[M, Env[PGCopyManager], ?] = new CopyManagerInterpreter { }
  lazy val CopyOutInterpreter: CopyOutOp ~> Kleisli[M, Env[PGCopyOut], ?] = new CopyOutInterpreter { }
  lazy val FastpathInterpreter: FastpathOp ~> Kleisli[M, Env[PGFastpath], ?] = new FastpathInterpreter { }
  lazy val LargeObjectInterpreter: LargeObjectOp ~> Kleisli[M, Env[LargeObject], ?] = new LargeObjectInterpreter { }
  lazy val LargeObjectManagerInterpreter: LargeObjectManagerOp ~> Kleisli[M, Env[LargeObjectManager], ?] = new LargeObjectManagerInterpreter { }
  lazy val PGConnectionInterpreter: PGConnectionOp ~> Kleisli[M, Env[PGConnection], ?] = new PGConnectionInterpreter { }

  // Some methods are common to all interpreters and can be overridden to change behavior globally.
  def primitive[J, A](f: J => A, name: String, args: Any*): Kleisli[M, Env[J], A] =
    raw(_.unsafeTrace(s"$name(${args.mkString(", ")})")(f))

  def delay[J, A](a: () => A): Kleisli[M, Env[J], A] =
    Kleisli(_ => asyncM.delay(a()))

  def raw[J, A](f: Env[J] => A): Kleisli[M, Env[J], A] =
    Kleisli(e => contextShiftM.evalOn(e.blockingContext)(asyncM.delay(f(e))))

  def async[J, A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[J], A] =
    Kleisli(_ => asyncM.async(k))

  def embed[J, A](e: Embedded[A]): Kleisli[M, Env[J], A] =
    e match {
      case Embedded.CopyIn(j, fa) => Kleisli(e => fa.foldMap(CopyInInterpreter).run(e.copy(jdbc = j)))
      case Embedded.CopyManager(j, fa) => Kleisli(e => fa.foldMap(CopyManagerInterpreter).run(e.copy(jdbc = j)))
      case Embedded.CopyOut(j, fa) => Kleisli(e => fa.foldMap(CopyOutInterpreter).run(e.copy(jdbc = j)))
      case Embedded.Fastpath(j, fa) => Kleisli(e => fa.foldMap(FastpathInterpreter).run(e.copy(jdbc = j)))
      case Embedded.LargeObject(j, fa) => Kleisli(e => fa.foldMap(LargeObjectInterpreter).run(e.copy(jdbc = j)))
      case Embedded.LargeObjectManager(j, fa) => Kleisli(e => fa.foldMap(LargeObjectManagerInterpreter).run(e.copy(jdbc = j)))
      case Embedded.PGConnection(j, fa) => Kleisli(e => fa.foldMap(PGConnectionInterpreter).run(e.copy(jdbc = j)))
    }

  // Interpreters
  trait CopyInInterpreter extends CopyInOp.Visitor[Kleisli[M, Env[PGCopyIn], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[PGCopyIn] => A): Kleisli[M, Env[PGCopyIn], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[PGCopyIn], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[PGCopyIn], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[PGCopyIn], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyInIO[Unit]): Kleisli[M, Env[PGCopyIn], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: CopyInIO[A], f: Throwable => CopyInIO[A]): Kleisli[M, Env[PGCopyIn], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: CopyInIO[A])(use: A => CopyInIO[B])(release: (A, ExitCase[Throwable]) => CopyInIO[Unit]): Kleisli[M, Env[PGCopyIn], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[PGCopyIn], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: CopyInIO[A]): Kleisli[M, Env[PGCopyIn], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[PGCopyIn] => G ~> CopyInIO): Kleisli[M, Env[PGCopyIn], G ~> CopyInIO] =
      Kleisli(e => asyncM.pure(env(e)))

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

  }

  trait CopyManagerInterpreter extends CopyManagerOp.Visitor[Kleisli[M, Env[PGCopyManager], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[PGCopyManager] => A): Kleisli[M, Env[PGCopyManager], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[PGCopyManager], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[PGCopyManager], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[PGCopyManager], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyManagerIO[Unit]): Kleisli[M, Env[PGCopyManager], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: CopyManagerIO[A], f: Throwable => CopyManagerIO[A]): Kleisli[M, Env[PGCopyManager], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: CopyManagerIO[A])(use: A => CopyManagerIO[B])(release: (A, ExitCase[Throwable]) => CopyManagerIO[Unit]): Kleisli[M, Env[PGCopyManager], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[PGCopyManager], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: CopyManagerIO[A]): Kleisli[M, Env[PGCopyManager], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[PGCopyManager] => G ~> CopyManagerIO): Kleisli[M, Env[PGCopyManager], G ~> CopyManagerIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def copyDual(a: String) = primitive(_.copyDual(a), "copyDual", a)
    override def copyIn(a: String) = primitive(_.copyIn(a), "copyIn", a)
    override def copyIn(a: String, b: InputStream) = primitive(_.copyIn(a, b), "copyIn", a, b)
    override def copyIn(a: String, b: InputStream, c: Int) = primitive(_.copyIn(a, b, c), "copyIn", a, b, c)
    override def copyIn(a: String, b: Reader) = primitive(_.copyIn(a, b), "copyIn", a, b)
    override def copyIn(a: String, b: Reader, c: Int) = primitive(_.copyIn(a, b, c), "copyIn", a, b, c)
    override def copyOut(a: String) = primitive(_.copyOut(a), "copyOut", a)
    override def copyOut(a: String, b: OutputStream) = primitive(_.copyOut(a, b), "copyOut", a, b)
    override def copyOut(a: String, b: Writer) = primitive(_.copyOut(a, b), "copyOut", a, b)

  }

  trait CopyOutInterpreter extends CopyOutOp.Visitor[Kleisli[M, Env[PGCopyOut], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[PGCopyOut] => A): Kleisli[M, Env[PGCopyOut], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[PGCopyOut], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[PGCopyOut], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[PGCopyOut], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => CopyOutIO[Unit]): Kleisli[M, Env[PGCopyOut], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: CopyOutIO[A], f: Throwable => CopyOutIO[A]): Kleisli[M, Env[PGCopyOut], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: CopyOutIO[A])(use: A => CopyOutIO[B])(release: (A, ExitCase[Throwable]) => CopyOutIO[Unit]): Kleisli[M, Env[PGCopyOut], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[PGCopyOut], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: CopyOutIO[A]): Kleisli[M, Env[PGCopyOut], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[PGCopyOut] => G ~> CopyOutIO): Kleisli[M, Env[PGCopyOut], G ~> CopyOutIO] =
      Kleisli(e => asyncM.pure(env(e)))

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

  trait FastpathInterpreter extends FastpathOp.Visitor[Kleisli[M, Env[PGFastpath], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[PGFastpath] => A): Kleisli[M, Env[PGFastpath], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[PGFastpath], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[PGFastpath], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[PGFastpath], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => FastpathIO[Unit]): Kleisli[M, Env[PGFastpath], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: FastpathIO[A], f: Throwable => FastpathIO[A]): Kleisli[M, Env[PGFastpath], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: FastpathIO[A])(use: A => FastpathIO[B])(release: (A, ExitCase[Throwable]) => FastpathIO[Unit]): Kleisli[M, Env[PGFastpath], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[PGFastpath], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: FastpathIO[A]): Kleisli[M, Env[PGFastpath], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[PGFastpath] => G ~> FastpathIO): Kleisli[M, Env[PGFastpath], G ~> FastpathIO] =
      Kleisli(e => asyncM.pure(env(e)))

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

  trait LargeObjectInterpreter extends LargeObjectOp.Visitor[Kleisli[M, Env[LargeObject], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[LargeObject] => A): Kleisli[M, Env[LargeObject], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[LargeObject], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[LargeObject], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[LargeObject], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => LargeObjectIO[Unit]): Kleisli[M, Env[LargeObject], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: LargeObjectIO[A], f: Throwable => LargeObjectIO[A]): Kleisli[M, Env[LargeObject], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: LargeObjectIO[A])(use: A => LargeObjectIO[B])(release: (A, ExitCase[Throwable]) => LargeObjectIO[Unit]): Kleisli[M, Env[LargeObject], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[LargeObject], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: LargeObjectIO[A]): Kleisli[M, Env[LargeObject], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[LargeObject] => G ~> LargeObjectIO): Kleisli[M, Env[LargeObject], G ~> LargeObjectIO] =
      Kleisli(e => asyncM.pure(env(e)))

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

  trait LargeObjectManagerInterpreter extends LargeObjectManagerOp.Visitor[Kleisli[M, Env[LargeObjectManager], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[LargeObjectManager] => A): Kleisli[M, Env[LargeObjectManager], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[LargeObjectManager], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[LargeObjectManager], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[LargeObjectManager], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => LargeObjectManagerIO[Unit]): Kleisli[M, Env[LargeObjectManager], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: LargeObjectManagerIO[A], f: Throwable => LargeObjectManagerIO[A]): Kleisli[M, Env[LargeObjectManager], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: LargeObjectManagerIO[A])(use: A => LargeObjectManagerIO[B])(release: (A, ExitCase[Throwable]) => LargeObjectManagerIO[Unit]): Kleisli[M, Env[LargeObjectManager], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[LargeObjectManager], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: LargeObjectManagerIO[A]): Kleisli[M, Env[LargeObjectManager], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[LargeObjectManager] => G ~> LargeObjectManagerIO): Kleisli[M, Env[LargeObjectManager], G ~> LargeObjectManagerIO] =
      Kleisli(e => asyncM.pure(env(e)))

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

  trait PGConnectionInterpreter extends PGConnectionOp.Visitor[Kleisli[M, Env[PGConnection], ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Env[PGConnection] => A): Kleisli[M, Env[PGConnection], A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Env[PGConnection], A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Env[PGConnection], A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Env[PGConnection], A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => PGConnectionIO[Unit]): Kleisli[M, Env[PGConnection], A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: PGConnectionIO[A], f: Throwable => PGConnectionIO[A]): Kleisli[M, Env[PGConnection], A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: PGConnectionIO[A])(use: A => PGConnectionIO[B])(release: (A, ExitCase[Throwable]) => PGConnectionIO[Unit]): Kleisli[M, Env[PGConnection], B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Env[PGConnection], Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: PGConnectionIO[A]): Kleisli[M, Env[PGConnection], A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    def liftE[G[_]](env: Env[PGConnection] => G ~> PGConnectionIO): Kleisli[M, Env[PGConnection], G ~> PGConnectionIO] =
      Kleisli(e => asyncM.pure(env(e)))

    // domain-specific operations are implemented in terms of `primitive`
    override def addDataType(a: String, b: Class[_ <: org.postgresql.util.PGobject]) = primitive(_.addDataType(a, b), "addDataType", a, b)
    override def addDataType(a: String, b: String) = primitive(_.addDataType(a, b), "addDataType", a, b)
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
    override def getPreferQueryMode = primitive(_.getPreferQueryMode, "getPreferQueryMode")
    override def getPrepareThreshold = primitive(_.getPrepareThreshold, "getPrepareThreshold")
    override def getReplicationAPI = primitive(_.getReplicationAPI, "getReplicationAPI")
    override def setAutosave(a: AutoSave) = primitive(_.setAutosave(a), "setAutosave", a)
    override def setDefaultFetchSize(a: Int) = primitive(_.setDefaultFetchSize(a), "setDefaultFetchSize", a)
    override def setPrepareThreshold(a: Int) = primitive(_.setPrepareThreshold(a), "setPrepareThreshold", a)

  }


}


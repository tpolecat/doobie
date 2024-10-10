// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// format: off

package doobie.postgres.free

// Library imports
import cats.~>
import cats.data.Kleisli
import cats.effect.kernel.{ Poll, Sync }
import cats.free.Free
import doobie.WeakAsync
import doobie.util.log.{LogEvent, LogHandler}
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

// Types referenced in the JDBC API
import java.io.InputStream
import java.io.OutputStream
import java.io.Reader
import java.io.Writer
import java.lang.Class
import java.lang.String
import org.postgresql.PGConnection
import org.postgresql.PGNotification
import org.postgresql.copy.{ CopyIn as PGCopyIn }
import org.postgresql.copy.{ CopyManager as PGCopyManager }
import org.postgresql.copy.{ CopyOut as PGCopyOut }
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
import doobie.postgres.free.largeobject.{ LargeObjectIO, LargeObjectOp }
import doobie.postgres.free.largeobjectmanager.{ LargeObjectManagerIO, LargeObjectManagerOp }
import doobie.postgres.free.pgconnection.{ PGConnectionIO, PGConnectionOp }

object KleisliInterpreter {
  def apply[M[_]: WeakAsync](logHandler: LogHandler[M]): KleisliInterpreter[M] =
    new KleisliInterpreter[M](logHandler)
}

// Family of interpreters into Kleisli arrows for some monad M.
class KleisliInterpreter[M[_]](logHandler: LogHandler[M])(implicit val asyncM: WeakAsync[M]) { outer =>

  // The 6 interpreters, with definitions below. These can be overridden to customize behavior.
  lazy val CopyInInterpreter: CopyInOp ~> Kleisli[M, PGCopyIn, *] = new CopyInInterpreter { }
  lazy val CopyManagerInterpreter: CopyManagerOp ~> Kleisli[M, PGCopyManager, *] = new CopyManagerInterpreter { }
  lazy val CopyOutInterpreter: CopyOutOp ~> Kleisli[M, PGCopyOut, *] = new CopyOutInterpreter { }
  lazy val LargeObjectInterpreter: LargeObjectOp ~> Kleisli[M, LargeObject, *] = new LargeObjectInterpreter { }
  lazy val LargeObjectManagerInterpreter: LargeObjectManagerOp ~> Kleisli[M, LargeObjectManager, *] = new LargeObjectManagerInterpreter { }
  lazy val PGConnectionInterpreter: PGConnectionOp ~> Kleisli[M, PGConnection, *] = new PGConnectionInterpreter { }

  // Some methods are common to all interpreters and can be overridden to change behavior globally.
  def primitive[J, A](f: J => A): Kleisli[M, J, A] = Kleisli { a =>
    // primitive JDBC methods throw exceptions and so do we when reading values
    // so catch any non-fatal exceptions and lift them into the effect
    try {
      asyncM.blocking(f(a))
    } catch {
      case scala.util.control.NonFatal(e) => asyncM.raiseError(e)
    }
  }
  def raw[J, A](f: J => A): Kleisli[M, J, A] = primitive(f)
  def raiseError[J, A](e: Throwable): Kleisli[M, J, A] = Kleisli(_ => asyncM.raiseError(e))
  def monotonic[J]: Kleisli[M, J, FiniteDuration] = Kleisli(_ => asyncM.monotonic)
  def realTime[J]: Kleisli[M, J, FiniteDuration] = Kleisli(_ => asyncM.realTime)
  def delay[J, A](thunk: => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.delay(thunk))
  def suspend[J, A](hint: Sync.Type)(thunk: => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.suspend(hint)(thunk))
  def canceled[J]: Kleisli[M, J, Unit] = Kleisli(_ => asyncM.canceled)

  // for operations using free structures we call the interpreter recursively
  def handleErrorWith[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A])(f: Throwable => Free[G, A]): Kleisli[M, J, A] = Kleisli (j =>
    asyncM.handleErrorWith(fa.foldMap(interpreter).run(j))(f.andThen(_.foldMap(interpreter).run(j)))
  )
  def forceR[G[_], J, A, B](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A])(fb: Free[G, B]): Kleisli[M, J, B] = Kleisli (j =>
    asyncM.forceR(fa.foldMap(interpreter).run(j))(fb.foldMap(interpreter).run(j))
  )
  def uncancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *], capture: Poll[M] => Poll[Free[G, *]])(body: Poll[Free[G, *]] => Free[G, A]): Kleisli[M, J, A] = Kleisli(j =>
    asyncM.uncancelable(body.compose(capture).andThen(_.foldMap(interpreter).run(j)))
  )
  def poll[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(mpoll: Any, fa: Free[G, A]): Kleisli[M, J, A] = Kleisli(j =>
    mpoll.asInstanceOf[Poll[M]].apply(fa.foldMap(interpreter).run(j))
  )
  def onCancel[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A], fin: Free[G, Unit]): Kleisli[M, J, A] = Kleisli (j =>
    asyncM.onCancel(fa.foldMap(interpreter).run(j), fin.foldMap(interpreter).run(j))
  )
  def fromFuture[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fut: Free[G, Future[A]]): Kleisli[M, J, A] = Kleisli(j =>
    asyncM.fromFuture(fut.foldMap(interpreter).run(j))
  )
  def fromFutureCancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fut: Free[G, (Future[A], Free[G, Unit])]): Kleisli[M, J, A] = Kleisli(j =>
    asyncM.fromFutureCancelable(fut.map { case (f, g) => (f, g.foldMap(interpreter).run(j)) }.foldMap(interpreter).run(j))
  )
  def cancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A], fin: Free[G, Unit]): Kleisli[M, J, A] = Kleisli (j =>
    asyncM.cancelable(fa.foldMap(interpreter).run(j), fin.foldMap(interpreter).run(j))
  )
  def embed[J, A](e: Embedded[A]): Kleisli[M, J, A] =
    e match {
      case Embedded.CopyIn(j, fa) => Kleisli(_ => fa.foldMap(CopyInInterpreter).run(j))
      case Embedded.CopyManager(j, fa) => Kleisli(_ => fa.foldMap(CopyManagerInterpreter).run(j))
      case Embedded.CopyOut(j, fa) => Kleisli(_ => fa.foldMap(CopyOutInterpreter).run(j))
      case Embedded.LargeObject(j, fa) => Kleisli(_ => fa.foldMap(LargeObjectInterpreter).run(j))
      case Embedded.LargeObjectManager(j, fa) => Kleisli(_ => fa.foldMap(LargeObjectManagerInterpreter).run(j))
      case Embedded.PGConnection(j, fa) => Kleisli(_ => fa.foldMap(PGConnectionInterpreter).run(j))
    }

  // Interpreters
  trait CopyInInterpreter extends CopyInOp.Visitor[Kleisli[M, PGCopyIn, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: PGCopyIn => A): Kleisli[M, PGCopyIn, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGCopyIn, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, PGCopyIn, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, PGCopyIn, FiniteDuration] = outer.monotonic[PGCopyIn]
    override def realTime: Kleisli[M, PGCopyIn, FiniteDuration] = outer.realTime[PGCopyIn]
    override def delay[A](thunk: => A): Kleisli[M, PGCopyIn, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, PGCopyIn, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, PGCopyIn, Unit] = outer.canceled[PGCopyIn]

    override def performLogging(event: LogEvent): Kleisli[M, PGCopyIn, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using CopyInIO we must call ourself recursively
    override def handleErrorWith[A](fa: CopyInIO[A])(f: Throwable => CopyInIO[A]): Kleisli[M, PGCopyIn, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: CopyInIO[A])(fb: CopyInIO[B]): Kleisli[M, PGCopyIn, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[CopyInIO] => CopyInIO[A]): Kleisli[M, PGCopyIn, A] = outer.uncancelable(this, doobie.postgres.free.copyin.capturePoll)(body)
    override def poll[A](poll: Any, fa: CopyInIO[A]): Kleisli[M, PGCopyIn, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: CopyInIO[A], fin: CopyInIO[Unit]): Kleisli[M, PGCopyIn, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: CopyInIO[Future[A]]): Kleisli[M, PGCopyIn, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: CopyInIO[(Future[A], CopyInIO[Unit])]): Kleisli[M, PGCopyIn, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: CopyInIO[A], fin: CopyInIO[Unit]): Kleisli[M, PGCopyIn, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def cancelCopy: Kleisli[M, PGCopyIn, Unit] = primitive(_.cancelCopy)
    override def endCopy: Kleisli[M, PGCopyIn, Long] = primitive(_.endCopy)
    override def flushCopy: Kleisli[M, PGCopyIn, Unit] = primitive(_.flushCopy)
    override def getFieldCount: Kleisli[M, PGCopyIn, Int] = primitive(_.getFieldCount)
    override def getFieldFormat(a: Int) = primitive(_.getFieldFormat(a))
    override def getFormat: Kleisli[M, PGCopyIn, Int] = primitive(_.getFormat)
    override def getHandledRowCount: Kleisli[M, PGCopyIn, Long] = primitive(_.getHandledRowCount)
    override def isActive: Kleisli[M, PGCopyIn, Boolean] = primitive(_.isActive)
    override def writeToCopy(a: Array[Byte], b: Int, c: Int) = primitive(_.writeToCopy(a, b, c))
    override def writeToCopy(a: ByteStreamWriter) = primitive(_.writeToCopy(a))

  }

  trait CopyManagerInterpreter extends CopyManagerOp.Visitor[Kleisli[M, PGCopyManager, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: PGCopyManager => A): Kleisli[M, PGCopyManager, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGCopyManager, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, PGCopyManager, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, PGCopyManager, FiniteDuration] = outer.monotonic[PGCopyManager]
    override def realTime: Kleisli[M, PGCopyManager, FiniteDuration] = outer.realTime[PGCopyManager]
    override def delay[A](thunk: => A): Kleisli[M, PGCopyManager, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, PGCopyManager, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, PGCopyManager, Unit] = outer.canceled[PGCopyManager]

    override def performLogging(event: LogEvent): Kleisli[M, PGCopyManager, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using CopyManagerIO we must call ourself recursively
    override def handleErrorWith[A](fa: CopyManagerIO[A])(f: Throwable => CopyManagerIO[A]): Kleisli[M, PGCopyManager, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: CopyManagerIO[A])(fb: CopyManagerIO[B]): Kleisli[M, PGCopyManager, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[CopyManagerIO] => CopyManagerIO[A]): Kleisli[M, PGCopyManager, A] = outer.uncancelable(this, doobie.postgres.free.copymanager.capturePoll)(body)
    override def poll[A](poll: Any, fa: CopyManagerIO[A]): Kleisli[M, PGCopyManager, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]): Kleisli[M, PGCopyManager, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: CopyManagerIO[Future[A]]): Kleisli[M, PGCopyManager, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: CopyManagerIO[(Future[A], CopyManagerIO[Unit])]): Kleisli[M, PGCopyManager, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: CopyManagerIO[A], fin: CopyManagerIO[Unit]): Kleisli[M, PGCopyManager, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def copyDual(a: String) = primitive(_.copyDual(a))
    override def copyIn(a: String) = primitive(_.copyIn(a))
    override def copyIn(a: String, b: ByteStreamWriter) = primitive(_.copyIn(a, b))
    override def copyIn(a: String, b: InputStream) = primitive(_.copyIn(a, b))
    override def copyIn(a: String, b: InputStream, c: Int) = primitive(_.copyIn(a, b, c))
    override def copyIn(a: String, b: Reader) = primitive(_.copyIn(a, b))
    override def copyIn(a: String, b: Reader, c: Int) = primitive(_.copyIn(a, b, c))
    override def copyOut(a: String) = primitive(_.copyOut(a))
    override def copyOut(a: String, b: OutputStream) = primitive(_.copyOut(a, b))
    override def copyOut(a: String, b: Writer) = primitive(_.copyOut(a, b))

  }

  trait CopyOutInterpreter extends CopyOutOp.Visitor[Kleisli[M, PGCopyOut, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: PGCopyOut => A): Kleisli[M, PGCopyOut, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGCopyOut, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, PGCopyOut, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, PGCopyOut, FiniteDuration] = outer.monotonic[PGCopyOut]
    override def realTime: Kleisli[M, PGCopyOut, FiniteDuration] = outer.realTime[PGCopyOut]
    override def delay[A](thunk: => A): Kleisli[M, PGCopyOut, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, PGCopyOut, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, PGCopyOut, Unit] = outer.canceled[PGCopyOut]

    override def performLogging(event: LogEvent): Kleisli[M, PGCopyOut, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using CopyOutIO we must call ourself recursively
    override def handleErrorWith[A](fa: CopyOutIO[A])(f: Throwable => CopyOutIO[A]): Kleisli[M, PGCopyOut, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: CopyOutIO[A])(fb: CopyOutIO[B]): Kleisli[M, PGCopyOut, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[CopyOutIO] => CopyOutIO[A]): Kleisli[M, PGCopyOut, A] = outer.uncancelable(this, doobie.postgres.free.copyout.capturePoll)(body)
    override def poll[A](poll: Any, fa: CopyOutIO[A]): Kleisli[M, PGCopyOut, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: CopyOutIO[A], fin: CopyOutIO[Unit]): Kleisli[M, PGCopyOut, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: CopyOutIO[Future[A]]): Kleisli[M, PGCopyOut, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: CopyOutIO[(Future[A], CopyOutIO[Unit])]): Kleisli[M, PGCopyOut, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: CopyOutIO[A], fin: CopyOutIO[Unit]): Kleisli[M, PGCopyOut, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def cancelCopy: Kleisli[M, PGCopyOut, Unit] = primitive(_.cancelCopy)
    override def getFieldCount: Kleisli[M, PGCopyOut, Int] = primitive(_.getFieldCount)
    override def getFieldFormat(a: Int) = primitive(_.getFieldFormat(a))
    override def getFormat: Kleisli[M, PGCopyOut, Int] = primitive(_.getFormat)
    override def getHandledRowCount: Kleisli[M, PGCopyOut, Long] = primitive(_.getHandledRowCount)
    override def isActive: Kleisli[M, PGCopyOut, Boolean] = primitive(_.isActive)
    override def readFromCopy: Kleisli[M, PGCopyOut, Array[Byte]] = primitive(_.readFromCopy)
    override def readFromCopy(a: Boolean) = primitive(_.readFromCopy(a))

  }

  trait LargeObjectInterpreter extends LargeObjectOp.Visitor[Kleisli[M, LargeObject, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: LargeObject => A): Kleisli[M, LargeObject, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, LargeObject, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, LargeObject, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, LargeObject, FiniteDuration] = outer.monotonic[LargeObject]
    override def realTime: Kleisli[M, LargeObject, FiniteDuration] = outer.realTime[LargeObject]
    override def delay[A](thunk: => A): Kleisli[M, LargeObject, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, LargeObject, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, LargeObject, Unit] = outer.canceled[LargeObject]

    override def performLogging(event: LogEvent): Kleisli[M, LargeObject, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using LargeObjectIO we must call ourself recursively
    override def handleErrorWith[A](fa: LargeObjectIO[A])(f: Throwable => LargeObjectIO[A]): Kleisli[M, LargeObject, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: LargeObjectIO[A])(fb: LargeObjectIO[B]): Kleisli[M, LargeObject, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[LargeObjectIO] => LargeObjectIO[A]): Kleisli[M, LargeObject, A] = outer.uncancelable(this, doobie.postgres.free.largeobject.capturePoll)(body)
    override def poll[A](poll: Any, fa: LargeObjectIO[A]): Kleisli[M, LargeObject, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]): Kleisli[M, LargeObject, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: LargeObjectIO[Future[A]]): Kleisli[M, LargeObject, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: LargeObjectIO[(Future[A], LargeObjectIO[Unit])]): Kleisli[M, LargeObject, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: LargeObjectIO[A], fin: LargeObjectIO[Unit]): Kleisli[M, LargeObject, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def close: Kleisli[M, LargeObject, Unit] = primitive(_.close)
    override def copy: Kleisli[M, LargeObject, LargeObject] = primitive(_.copy)
    override def getInputStream: Kleisli[M, LargeObject, InputStream] = primitive(_.getInputStream)
    override def getInputStream(a: Int, b: Long) = primitive(_.getInputStream(a, b))
    override def getInputStream(a: Long) = primitive(_.getInputStream(a))
    override def getLongOID: Kleisli[M, LargeObject, Long] = primitive(_.getLongOID)
    override def getOutputStream: Kleisli[M, LargeObject, OutputStream] = primitive(_.getOutputStream)
    override def read(a: Array[Byte], b: Int, c: Int) = primitive(_.read(a, b, c))
    override def read(a: Int) = primitive(_.read(a))
    override def seek(a: Int) = primitive(_.seek(a))
    override def seek(a: Int, b: Int) = primitive(_.seek(a, b))
    override def seek64(a: Long, b: Int) = primitive(_.seek64(a, b))
    override def size: Kleisli[M, LargeObject, Int] = primitive(_.size)
    override def size64: Kleisli[M, LargeObject, Long] = primitive(_.size64)
    override def tell: Kleisli[M, LargeObject, Int] = primitive(_.tell)
    override def tell64: Kleisli[M, LargeObject, Long] = primitive(_.tell64)
    override def truncate(a: Int) = primitive(_.truncate(a))
    override def truncate64(a: Long) = primitive(_.truncate64(a))
    override def write(a: Array[Byte]) = primitive(_.write(a))
    override def write(a: Array[Byte], b: Int, c: Int) = primitive(_.write(a, b, c))
    override def write(a: ByteStreamWriter) = primitive(_.write(a))

  }

  trait LargeObjectManagerInterpreter extends LargeObjectManagerOp.Visitor[Kleisli[M, LargeObjectManager, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: LargeObjectManager => A): Kleisli[M, LargeObjectManager, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, LargeObjectManager, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, LargeObjectManager, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, LargeObjectManager, FiniteDuration] = outer.monotonic[LargeObjectManager]
    override def realTime: Kleisli[M, LargeObjectManager, FiniteDuration] = outer.realTime[LargeObjectManager]
    override def delay[A](thunk: => A): Kleisli[M, LargeObjectManager, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, LargeObjectManager, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, LargeObjectManager, Unit] = outer.canceled[LargeObjectManager]

    override def performLogging(event: LogEvent): Kleisli[M, LargeObjectManager, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using LargeObjectManagerIO we must call ourself recursively
    override def handleErrorWith[A](fa: LargeObjectManagerIO[A])(f: Throwable => LargeObjectManagerIO[A]): Kleisli[M, LargeObjectManager, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: LargeObjectManagerIO[A])(fb: LargeObjectManagerIO[B]): Kleisli[M, LargeObjectManager, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[LargeObjectManagerIO] => LargeObjectManagerIO[A]): Kleisli[M, LargeObjectManager, A] = outer.uncancelable(this, doobie.postgres.free.largeobjectmanager.capturePoll)(body)
    override def poll[A](poll: Any, fa: LargeObjectManagerIO[A]): Kleisli[M, LargeObjectManager, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]): Kleisli[M, LargeObjectManager, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: LargeObjectManagerIO[Future[A]]): Kleisli[M, LargeObjectManager, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: LargeObjectManagerIO[(Future[A], LargeObjectManagerIO[Unit])]): Kleisli[M, LargeObjectManager, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: LargeObjectManagerIO[A], fin: LargeObjectManagerIO[Unit]): Kleisli[M, LargeObjectManager, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def createLO: Kleisli[M, LargeObjectManager, Long] = primitive(_.createLO)
    override def createLO(a: Int) = primitive(_.createLO(a))
    override def delete(a: Long) = primitive(_.delete(a))
    override def open(a: Int, b: Boolean) = primitive(_.open(a, b))
    override def open(a: Int, b: Int, c: Boolean) = primitive(_.open(a, b, c))
    override def open(a: Long) = primitive(_.open(a))
    override def open(a: Long, b: Boolean) = primitive(_.open(a, b))
    override def open(a: Long, b: Int) = primitive(_.open(a, b))
    override def open(a: Long, b: Int, c: Boolean) = primitive(_.open(a, b, c))
    override def unlink(a: Long) = primitive(_.unlink(a))

  }

  trait PGConnectionInterpreter extends PGConnectionOp.Visitor[Kleisli[M, PGConnection, *]] {

    // common operations delegate to outer interpreter
    override def raw[A](f: PGConnection => A): Kleisli[M, PGConnection, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, PGConnection, A] = outer.embed(e)
    override def raiseError[A](e: Throwable): Kleisli[M, PGConnection, A] = outer.raiseError(e)
    override def monotonic: Kleisli[M, PGConnection, FiniteDuration] = outer.monotonic[PGConnection]
    override def realTime: Kleisli[M, PGConnection, FiniteDuration] = outer.realTime[PGConnection]
    override def delay[A](thunk: => A): Kleisli[M, PGConnection, A] = outer.delay(thunk)
    override def suspend[A](hint: Sync.Type)(thunk: => A): Kleisli[M, PGConnection, A] = outer.suspend(hint)(thunk)
    override def canceled: Kleisli[M, PGConnection, Unit] = outer.canceled[PGConnection]

    override def performLogging(event: LogEvent): Kleisli[M, PGConnection, Unit] = Kleisli(_ => logHandler.run(event))

    // for operations using PGConnectionIO we must call ourself recursively
    override def handleErrorWith[A](fa: PGConnectionIO[A])(f: Throwable => PGConnectionIO[A]): Kleisli[M, PGConnection, A] = outer.handleErrorWith(this)(fa)(f)
    override def forceR[A, B](fa: PGConnectionIO[A])(fb: PGConnectionIO[B]): Kleisli[M, PGConnection, B] = outer.forceR(this)(fa)(fb)
    override def uncancelable[A](body: Poll[PGConnectionIO] => PGConnectionIO[A]): Kleisli[M, PGConnection, A] = outer.uncancelable(this, doobie.postgres.free.pgconnection.capturePoll)(body)
    override def poll[A](poll: Any, fa: PGConnectionIO[A]): Kleisli[M, PGConnection, A] = outer.poll(this)(poll, fa)
    override def onCancel[A](fa: PGConnectionIO[A], fin: PGConnectionIO[Unit]): Kleisli[M, PGConnection, A] = outer.onCancel(this)(fa, fin)
    override def fromFuture[A](fut: PGConnectionIO[Future[A]]): Kleisli[M, PGConnection, A] = outer.fromFuture(this)(fut)
    override def fromFutureCancelable[A](fut: PGConnectionIO[(Future[A], PGConnectionIO[Unit])]): Kleisli[M, PGConnection, A] = outer.fromFutureCancelable(this)(fut)
    override def cancelable[A](fa: PGConnectionIO[A], fin: PGConnectionIO[Unit]): Kleisli[M, PGConnection, A] = outer.cancelable(this)(fa, fin)


    // domain-specific operations are implemented in terms of `primitive`
    override def addDataType(a: String, b: Class[? <: org.postgresql.util.PGobject]) = primitive(_.addDataType(a, b))
    override def alterUserPassword(a: String, b: Array[Char], c: String) = primitive(_.alterUserPassword(a, b, c))
    override def cancelQuery: Kleisli[M, PGConnection, Unit] = primitive(_.cancelQuery)
    override def createArrayOf(a: String, b: AnyRef) = primitive(_.createArrayOf(a, b))
    override def escapeIdentifier(a: String) = primitive(_.escapeIdentifier(a))
    override def escapeLiteral(a: String) = primitive(_.escapeLiteral(a))
    override def getAdaptiveFetch: Kleisli[M, PGConnection, Boolean] = primitive(_.getAdaptiveFetch)
    override def getAutosave: Kleisli[M, PGConnection, AutoSave] = primitive(_.getAutosave)
    override def getBackendPID: Kleisli[M, PGConnection, Int] = primitive(_.getBackendPID)
    override def getCopyAPI: Kleisli[M, PGConnection, PGCopyManager] = primitive(_.getCopyAPI)
    override def getDefaultFetchSize: Kleisli[M, PGConnection, Int] = primitive(_.getDefaultFetchSize)
    override def getLargeObjectAPI: Kleisli[M, PGConnection, LargeObjectManager] = primitive(_.getLargeObjectAPI)
    override def getNotifications: Kleisli[M, PGConnection, Array[PGNotification]] = primitive(_.getNotifications)
    override def getNotifications(a: Int) = primitive(_.getNotifications(a))
    override def getParameterStatus(a: String) = primitive(_.getParameterStatus(a))
    override def getParameterStatuses: Kleisli[M, PGConnection, java.util.Map[String, String]] = primitive(_.getParameterStatuses)
    override def getPreferQueryMode: Kleisli[M, PGConnection, PreferQueryMode] = primitive(_.getPreferQueryMode)
    override def getPrepareThreshold: Kleisli[M, PGConnection, Int] = primitive(_.getPrepareThreshold)
    override def getReplicationAPI: Kleisli[M, PGConnection, PGReplicationConnection] = primitive(_.getReplicationAPI)
    override def setAdaptiveFetch(a: Boolean) = primitive(_.setAdaptiveFetch(a))
    override def setAutosave(a: AutoSave) = primitive(_.setAutosave(a))
    override def setDefaultFetchSize(a: Int) = primitive(_.setDefaultFetchSize(a))
    override def setPrepareThreshold(a: Int) = primitive(_.setPrepareThreshold(a))

  }


}


// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

import cats.~>
import cats.implicits._
import cats.effect.{IO, LiftIO}
import cats.effect.kernel.{Async, Poll, Resource, Sync}
import cats.effect.std.Dispatcher
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait WeakAsync[F[_]] extends Sync[F] {
  def fromFuture[A](fut: F[Future[A]]): F[A]
  def fromFutureCancelable[A](fut: F[(Future[A], F[Unit])]): F[A]
}

object WeakAsync {

  def apply[F[_]](implicit ev: WeakAsync[F]): WeakAsync[F] = ev

  implicit def doobieWeakAsyncForAsync[F[_]](implicit F: Async[F]): WeakAsync[F] =
    new WeakAsync[F] {
      override val applicative = F.applicative
      override val rootCancelScope = F.rootCancelScope
      override def pure[A](x: A): F[A] = F.pure(x)
      override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
      override def raiseError[A](e: Throwable): F[A] = F.raiseError(e)
      override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] = F.handleErrorWith(fa)(f)
      override def monotonic: F[FiniteDuration] = F.monotonic
      override def realTime: F[FiniteDuration] = F.realTime
      override def suspend[A](hint: Sync.Type)(thunk: => A): F[A] = F.suspend(hint)(thunk)
      override def forceR[A, B](fa: F[A])(fb: F[B]): F[B] = F.forceR(fa)(fb)
      override def uncancelable[A](body: Poll[F] => F[A]): F[A] = F.uncancelable(body)
      override def canceled: F[Unit] = F.canceled
      override def onCancel[A](fa: F[A], fin: F[Unit]): F[A] = F.onCancel(fa, fin)
      override def fromFuture[A](fut: F[Future[A]]): F[A] = F.fromFuture(fut)
      override def fromFutureCancelable[A](fut: F[(Future[A], F[Unit])]): F[A] = F.fromFutureCancelable(fut)
    }

  /** Create a natural transformation for lifting an `Async` effect `F` into a `WeakAsync` effect `G`
    * `cats.effect.std.Dispatcher` the trasformation is based on is stateful and requires finalization. Leaking it from
    * it's resource scope will lead to erorrs at runtime.
    */
  def liftK[F[_], G[_]](implicit F: Async[F], G: WeakAsync[G]): Resource[F, F ~> G] =
    Dispatcher.parallel[F].map(new Lifter(_))

  /** Like [[liftK]] but specifically returns a [[LiftIO]] */
  def liftIO[F[_]](implicit F: WeakAsync[F]): Resource[IO, LiftIO[F]] =
    Dispatcher.parallel[IO].map(new Lifter(_) with LiftIO[F] {
      def liftIO[A](ioa: IO[A]) = super[Lifter].apply(ioa)
    })

  private class Lifter[F[_], G[_]](dispatcher: Dispatcher[F])(implicit F: Async[F], G: WeakAsync[G]) extends (F ~> G) {
    def apply[T](fa: F[T]) = // first try to interpret directly into G, then fallback to the Dispatcher
      F.syncStep[G, T](fa, Int.MaxValue).flatMap { // MaxValue b/c we assume G will implement ceding/fairness
        case Left(fa) =>
          G.fromFutureCancelable {
            G.uncancelable { _ =>
              G.delay(dispatcher.unsafeToFutureCancelable(fa)).map { case (fut, cancel) =>
                (fut, G.fromFuture(G.delay(cancel())))
              }
            }
          }
        case Right(a) => G.pure(a)
      }
  }

}

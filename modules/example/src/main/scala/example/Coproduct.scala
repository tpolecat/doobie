// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package example

import java.sql.Connection

import cats.data.{EitherK, Kleisli}
import cats.effect.{ Blocker, IO, IOApp, ExitCode }
import cats.free.Free
import cats.implicits._
import cats.{InjectK, ~>}
import doobie._
import doobie.free.connection.ConnectionOp
import doobie.implicits._
import doobie.util.DefaultMessageLogger._
import scala.concurrent.ExecutionContext
import scala.io.StdIn

object coproduct extends IOApp {

  // This is merged in cats
  implicit class MoreFreeOps[F[_], A](fa: Free[F, A]) {
    def inject[G[_]](implicit ev: InjectK[F, G]): Free[G, A] =
      fa.compile(λ[F ~> G](ev.inj(_)))
  }

  // This is kind of eh … we need to interpret into Kleisli so this is helpful
  implicit class MoreNaturalTransformationOps[F[_], G[_]](nat: F ~> G) {
    def liftK[E] = λ[F ~> Kleisli[G, E, *]](fa => Kleisli(_ => nat(fa)))
  }

  // A console algebra
  sealed trait ConsoleOp[A]
  case object ReadLn             extends ConsoleOp[String]
  final case class PrintLn(s: String) extends ConsoleOp[Unit]

  // A module of ConsoleOp constructors, parameterized over a coproduct
  class ConsoleOps[F[_]](implicit ev: InjectK[ConsoleOp, F]) {
    val readLn             = Free.inject[ConsoleOp, F](ReadLn)
    def printLn(s: String) = Free.inject[ConsoleOp, F](PrintLn(s))
  }
  object ConsoleOps {
    implicit def instance[F[_]](implicit ev: InjectK[ConsoleOp, F]): ConsoleOps[F] = new ConsoleOps
  }

  // An interpreter into IO
  val consoleInterp = λ[ConsoleOp ~> IO] {
    case ReadLn     => IO(StdIn.readLine())
    case PrintLn(s) => IO(Console.println(s))
  }

  // A module of ConnectionOp programs, parameterized over a coproduct. The trick here is that these
  // are domain-specific operations that are injected as programs, not as constructors (which would
  // work but is too low-level to be useful).
  class ConnectionOps[F[_]](implicit ev: InjectK[ConnectionOp, F]) {
    def select(pat: String): Free[F, List[String]] =
      sql"select name from country where name like $pat".query[String].to[List].inject[F]
  }
  object ConnectionOps {
    implicit def instance[F[_]](implicit ev: InjectK[ConnectionOp, F]): ConnectionOps[F] = new ConnectionOps
  }

  // A program
  def prog[F[_]](implicit ev1: ConsoleOps[F], ev2: ConnectionOps[F]): Free[F, Unit] = {
    import ev1._
    import ev2._
    for {
      _   <- printLn("Enter a pattern:")
      pat <- readLn
      ns  <- select(pat)
      _   <- ns.traverse(printLn)
    } yield ()
  }

  // Our coproduct
  type Cop[A] = EitherK[ConsoleOp, ConnectionOp, A]

  // Our interpreter must be parameterized over a connection so we can add transaction boundaries
  // before and after.
  val interp: Cop ~> Kleisli[IO, Connection, *] = {
    val blocker = Blocker.liftExecutionContext(ExecutionContext.global)
    consoleInterp.liftK[Connection] or KleisliInterpreter[IO](blocker).ConnectionInterpreter
  }

  // Our interpreted program
  val iprog: Kleisli[IO, Connection, Unit] = prog[Cop].foldMap(interp)

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:world",
    "postgres", ""
  )

  // Exec it!
  def run(args: List[String]): IO[ExitCode] =
    xa.exec.apply(iprog).as(ExitCode.Success)

  // Enter a pattern:
  // U%
  // United Arab Emirates
  // United Kingdom
  // Uganda
  // Ukraine
  // Uruguay
  // Uzbekistan
  // United States
  // United States Minor Outlying Islands

}

// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.typelevel.doobie.util

import cats.effect.kernel.Async
import cats.instances.long.*
import cats.instances.string.*
import cats.syntax.show.*
import org.typelevel.doobie.free.connection.{ConnectionIO, delay}
import org.typelevel.doobie.implicits.*
import org.typelevel.doobie.util.query.*
import org.typelevel.doobie.util.update.*
import org.typelevel.doobie.util.testing.*
import org.typelevel.doobie.util.transactor.*
import fs2.Stream
import scala.Predef.*
import org.tpolecat.typename.*

/** Module for implicit syntax useful in REPL session. */

object yolo {

  class Yolo[M[_]](xa: Transactor[M])(implicit ev: Async[M]) {

    private def out(s: String, colors: Colors): ConnectionIO[Unit] =
      delay(Console.println(show"${colors.BLUE}  $s${colors.RESET}"))

    implicit class Query0YoloOps[A: TypeName](q: Query0[A]) {

      def quick(implicit colors: Colors = Colors.Ansi): M[Unit] =
        q.stream
          .map(_.toString)
          .evalMap(out(_, colors))
          .compile
          .drain
          .transact(xa)(using ev)

      def check(implicit colors: Colors = Colors.Ansi): M[Unit] =
        checkImpl(Analyzable.unpack(q), colors)

      def checkOutput(implicit colors: Colors = Colors.Ansi): M[Unit] =
        checkImpl(
          AnalysisArgs(
            show"Query0[${typeName[A]}]",
            q.pos,
            q.sql,
            q.outputAnalysis
          ),
          colors)
    }

    implicit class QueryYoloOps[I: TypeName, A: TypeName](q: Query[I, A]) {

      def quick(i: I): M[Unit] =
        q.toQuery0(i).quick

      def check(implicit colors: Colors = Colors.Ansi): M[Unit] =
        checkImpl(Analyzable.unpack(q), colors)

      def checkOutput(implicit colors: Colors = Colors.Ansi): M[Unit] =
        checkImpl(
          AnalysisArgs(
            show"Query[${typeName[I]}, ${typeName[A]}]",
            q.pos,
            q.sql,
            q.outputAnalysis
          ),
          colors)
    }

    implicit class Update0YoloOps(u: Update0) {

      def quick(implicit colors: Colors = Colors.Ansi): M[Unit] =
        u.run.flatMap(a => out(show"$a row(s) updated", colors)).transact(xa)(using ev)

      def check(implicit colors: Colors = Colors.Ansi): M[Unit] =
        checkImpl(Analyzable.unpack(u), colors)
    }

    implicit class UpdateYoloOps[I: TypeName](u: Update[I]) {

      def quick(i: I)(implicit colors: Colors = Colors.Ansi): M[Unit] =
        u.toUpdate0(i).quick

      def check(implicit colors: Colors = Colors.Ansi): M[Unit] =
        checkImpl(Analyzable.unpack(u), colors)
    }

    implicit class ConnectionIOYoloOps[A](ca: ConnectionIO[A]) {
      def quick(implicit colors: Colors = Colors.Ansi): M[Unit] =
        ca.flatMap(a => out(a.toString, colors)).transact(xa)(using ev)
    }

    implicit class StreamYoloOps[A](pa: Stream[ConnectionIO, A]) {
      def quick(implicit colors: Colors = Colors.Ansi): M[Unit] =
        pa.evalMap(a => out(a.toString, colors)).compile.drain.transact(xa)(using ev)
    }

    private def checkImpl(args: AnalysisArgs, colors: Colors): M[Unit] =
      analyze(args).flatMap { report =>
        val formatted = formatReport(args, report, colors)
        delay(println(formatted.padLeft("  ")))
      }.transact(xa)(using ev)
  }
}

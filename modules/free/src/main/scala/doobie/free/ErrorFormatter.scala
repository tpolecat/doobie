// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.free

import cats.implicits._
import scala.Predef._
import java.sql._

object ErrorFormatter {

  def indented(a: Any): String =
    a.toString.linesIterator.mkString("   ", "\n   ", "")

  def describe(jdbcObj: Any): String = {
    def it(kind: String): String =
      s"a $kind (${jdbcObj.getClass.getName}):\n\n${indented(jdbcObj)}\n\n"
    jdbcObj match {
      case _ : Blob                     => it("Blob")
      case _ : NClob                    => it("NClob")
      case _ : Clob                     => it("Clob")
      case _ : Connection               => it("Connection")
      case _ : Ref                      => it("Ref")
      case _ : SQLData                  => it("SQLData")
      case _ : SQLInput                 => it("SQLInput")
      case _ : SQLOutput                => it("SQLOutput")
      case md: DatabaseMetaData         => it("DatabaseMetaData")  + describe(md.getConnection())
      case rs: ResultSet                => it("ResultSet")         + describe(rs.getStatement())
      case st: CallableStatement        => it("CallableStatement") + describe(st.getConnection())
      case st: PreparedStatement        => it("PreparedStatement") + describe(st.getConnection())
      case st: Statement                => it("Statement")         + describe(st.getConnection())
      case o                    => it(o.getClass.getSimpleName)
    }
  }

  def format(jdbcObj: Any, operation: String, args: Seq[Any], error: Throwable): String = {

    def op: String =
      s"${operation}${if (args.isEmpty) "" else args.mkString("(", ",", ")")}".linesIterator.map(s => s"   $s").mkString("\n")

    def errorMessage(t: Throwable): String = {
      s"""|
          |A JDBC operation failed:
          |
          |$op
          |
          |The relevant JDBC object(s) were:
          |
          |${describe(jdbcObj).linesIterator.map(s => s"   $s").mkString("\n")}
          |The exception (${t.getClass().getName()}) was:
          |
          |${indented(t.getMessage())}
          |${Option(t.getCause).foldMap(t => s"\nThe underlying cause (${t.getClass().getName()}) was:\n\n${indented(t.getMessage)}\n")}
          |"""
        .stripMargin
        .linesIterator.map(s => s"🔥  $s").mkString("\n\n","\n","\n")
    }

    errorMessage(error)

  }

}
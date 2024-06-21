// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// format: off

package doobie.free

import cats.free.Free

import nclob.NClobIO
import blob.BlobIO
import clob.ClobIO
import databasemetadata.DatabaseMetaDataIO
import driver.DriverIO
import ref.RefIO
import sqldata.SQLDataIO
import sqlinput.SQLInputIO
import sqloutput.SQLOutputIO
import connection.ConnectionIO
import statement.StatementIO
import preparedstatement.PreparedStatementIO
import callablestatement.CallableStatementIO
import resultset.ResultSetIO

// A pair (J, Free[F, A]) with constructors that tie down J and F.
sealed trait Embedded[A]

object Embedded {
  final case class NClob[A](j: java.sql.NClob, fa: NClobIO[A]) extends Embedded[A]
  final case class Blob[A](j: java.sql.Blob, fa: BlobIO[A]) extends Embedded[A]
  final case class Clob[A](j: java.sql.Clob, fa: ClobIO[A]) extends Embedded[A]
  final case class DatabaseMetaData[A](j: java.sql.DatabaseMetaData, fa: DatabaseMetaDataIO[A]) extends Embedded[A]
  final case class Driver[A](j: java.sql.Driver, fa: DriverIO[A]) extends Embedded[A]
  final case class Ref[A](j: java.sql.Ref, fa: RefIO[A]) extends Embedded[A]
  final case class SQLData[A](j: java.sql.SQLData, fa: SQLDataIO[A]) extends Embedded[A]
  final case class SQLInput[A](j: java.sql.SQLInput, fa: SQLInputIO[A]) extends Embedded[A]
  final case class SQLOutput[A](j: java.sql.SQLOutput, fa: SQLOutputIO[A]) extends Embedded[A]
  final case class Connection[A](j: java.sql.Connection, fa: ConnectionIO[A]) extends Embedded[A]
  final case class Statement[A](j: java.sql.Statement, fa: StatementIO[A]) extends Embedded[A]
  final case class PreparedStatement[A](j: java.sql.PreparedStatement, fa: PreparedStatementIO[A]) extends Embedded[A]
  final case class CallableStatement[A](j: java.sql.CallableStatement, fa: CallableStatementIO[A]) extends Embedded[A]
  final case class ResultSet[A](j: java.sql.ResultSet, fa: ResultSetIO[A]) extends Embedded[A]
}

// Typeclass for embeddable pairs (J, F)
trait Embeddable[F[_], J] {
  def embed[A](j: J, fa: Free[F, A]): Embedded[A]
}


// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import doobie.enumerated.Nullability
import doobie.enumerated.Nullability._
import doobie.enumerated.ParameterMode
import doobie.enumerated.JdbcType
import doobie.util.pretty._

import scala.Predef._ // TODO: minimize

import cats.implicits._
import cats.data.Ior

/** Module defining a type for analyzing the type alignment of prepared statements. */
object analysis {

  /** Metadata for the JDBC end of a column/parameter mapping. */
  final case class ColumnMeta(jdbcType: JdbcType, vendorTypeName: String, nullability: Nullability, name: String)

  /** Metadata for the JDBC end of a column/parameter mapping. */
  final case class ParameterMeta(jdbcType: JdbcType, vendorTypeName: String, nullability: Nullability, mode: ParameterMode)

  sealed trait AlignmentError extends Product with Serializable {
    def tag: String
    def index: Int
    def msg: String
  }

  final case class ParameterMisalignment(index: Int, alignment: Option[ParameterMeta]) extends AlignmentError {
    val tag = "P"
    override def msg = this match {
      case ParameterMisalignment(_, None) =>
        s"""|Interpolated value has no corresponding SQL parameter and likely appears inside a
            |comment or quoted string. Ior.Left will result in a runtime failure; fix this by removing
            |the parameter.""".stripMargin.linesIterator.mkString(" ")
      case ParameterMisalignment(_, Some(pm)) =>
        s"""|${pm.jdbcType.show.toUpperCase} parameter is not set; this will result in a runtime
            |failure. Perhaps you used a literal ? rather than an interpolated value.""".stripMargin.linesIterator.mkString(" ")
    }
  }

  final case class ParameterTypeError(index: Int, put: Put[_], n: NullabilityKnown, jdbcType: JdbcType, vendorTypeName: String) extends AlignmentError {
    override val tag = "P"
    override def msg =
      s"""|${typeName(put.typeStack.last, n)} is not coercible to ${jdbcType.show.toUpperCase}
          |(${vendorTypeName})
          |according to the JDBC specification.
          |Expected schema type was ${put.jdbcTargets.head.show.toUpperCase}.""".stripMargin.linesIterator.mkString(" ")
  }

  final case class ColumnMisalignment(index: Int, alignment: Either[(Get[_], NullabilityKnown), ColumnMeta]) extends AlignmentError {
    override val tag = "C"
    override def msg = this match {
      case ColumnMisalignment(_, Left((get, n))) =>
        s"""|Too few columns are selected, which will result in a runtime failure. Add a column or
            |remove mapped ${typeName(get.typeStack.last, n)} from the result type.""".stripMargin.linesIterator.mkString(" ")
      case ColumnMisalignment(_, Right(_)) =>
        s"""Column is unused. Remove it from the SELECT statement."""
    }
  }

  final case class NullabilityMisalignment(index: Int, name: String, st: Option[String], jdk: NullabilityKnown, jdbc: NullabilityKnown) extends AlignmentError {
    override val tag = "C"
    override def msg = this match {
      // https://github.com/tpolecat/doobie/issues/164 ... NoNulls means "maybe no nulls"  :-\
      // case NullabilityMisalignment(i, name, st, NoNulls, Nullable) =>
      //   s"""Non-nullable column ${name.toUpperCase} is unnecessarily mapped to an Option type."""
      case NullabilityMisalignment(_, _, st, Nullable, NoNulls) =>
        s"""|Reading a NULL value into ${typeName(st, NoNulls)} will result in a runtime failure.
            |Fix this by making the schema type ${formatNullability(NoNulls)} or by changing the
            |Scala type to ${typeName(st, Nullable)}""".stripMargin.linesIterator.mkString(" ")
      case _ => sys.error("unpossible, evidently")
    }
  }

  final case class ColumnTypeError(index: Int, get: Get[_], n: NullabilityKnown, schema: ColumnMeta) extends AlignmentError {
    override val tag = "C"
    override def msg =
      s"""|${schema.jdbcType.show.toUpperCase} (${schema.vendorTypeName}) is not
          |coercible to ${typeName(get.typeStack.last, n)} according to the JDBC specification or any defined
          |mapping.
          |Fix this by changing the schema type to
          |${get.jdbcSources.toList.map(_.show.toUpperCase).toList.mkString(" or ") }; or the
          |Scala type to an appropriate ${if (schema.jdbcType === JdbcType.Array) "array" else "object"}
          |type.
          |""".stripMargin.linesIterator.mkString(" ")
  }

  final case class ColumnTypeWarning(index: Int, get: Get[_], n: NullabilityKnown, schema: ColumnMeta) extends AlignmentError {
    override val tag = "C"
    override def msg =
      s"""|${schema.jdbcType.show.toUpperCase} (${schema.vendorTypeName}) is ostensibly
          |coercible to ${typeName(get.typeStack.last, n)}
          |according to the JDBC specification but is not a recommended target type.
          |Expected schema type was
          |${get.jdbcSources.toList.map(_.show.toUpperCase).toList.mkString(" or ") }.
          |""".stripMargin.linesIterator.mkString(" ")
  }

  /** Compatibility analysis for the given statement and aligned mappings. */
  final case class Analysis(
    driver:             String,
    sql:                String,
    parameterAlignment: List[(Put[_], NullabilityKnown) Ior ParameterMeta],
    columnAlignment:    List[(Get[_], NullabilityKnown) Ior ColumnMeta]
  ) {

    private val parameterAlignment_ = parameterAlignment.map(_.map { m =>
      m.copy(jdbcType = tweakMetaJdbcType(driver, m.jdbcType, vendorTypeName = m.vendorTypeName))
    })
    private val columnAlignment_ = columnAlignment.map(_.map { m =>
      m.copy(jdbcType = tweakMetaJdbcType(driver, m.jdbcType, vendorTypeName = m.vendorTypeName))
    })

    def parameterMisalignments: List[ParameterMisalignment] =
      parameterAlignment_.zipWithIndex.collect {
        case (Ior.Left(_), n) => ParameterMisalignment(n + 1, None)
        case (Ior.Right(p), n) => ParameterMisalignment(n + 1, Some(p))
      }

    def parameterTypeErrors: List[ParameterTypeError] =
      parameterAlignment_.zipWithIndex.collect {
        case (Ior.Both((j, n1), p), n) if !j.jdbcTargets.contains_(p.jdbcType) =>
          ParameterTypeError(n + 1, j, n1, p.jdbcType, p.vendorTypeName)
      }

    def columnMisalignments: List[ColumnMisalignment] =
      columnAlignment_.zipWithIndex.collect {
        case (Ior.Left(j), n) => ColumnMisalignment(n + 1, Left(j))
        case (Ior.Right(p), n) => ColumnMisalignment(n + 1, Right(p))
      }

    def columnTypeErrors: List[ColumnTypeError] =
      columnAlignment.zipWithIndex.collect {
        case (Ior.Both((j, n1), p), n) if !(j.jdbcSources.toList ++ j.jdbcSourceSecondary).contains_(p.jdbcType) =>
          ColumnTypeError(n + 1, j, n1, p)
        case (Ior.Both((j, n1), p), n) if (p.jdbcType === JdbcType.JavaObject || p.jdbcType === JdbcType.Other) && !j.schemaTypes.headOption.contains_(p.vendorTypeName) =>
          ColumnTypeError(n + 1, j, n1, p)
      }

    def columnTypeWarnings: List[ColumnTypeWarning] =
      columnAlignment.zipWithIndex.collect {
        case (Ior.Both((j, n1), p), n) if j.jdbcSourceSecondary.contains_(p.jdbcType) =>
          ColumnTypeWarning(n + 1, j, n1, p)
      }

    def nullabilityMisalignments: List[NullabilityMisalignment] =
      columnAlignment_.zipWithIndex.collect {
        // We can't do anything helpful with NoNulls .. it means "might not be nullable"
        // case (Ior.Both((st, Nullable), ColumnMeta(_, _, NoNulls, col)), n) => NullabilityMisalignment(n + 1, col, st, NoNulls, Nullable)
        case (Ior.Both((st, NoNulls), ColumnMeta(_, _, Nullable, col)), n) => NullabilityMisalignment(n + 1, col, st.typeStack.last, Nullable, NoNulls)
        // N.B. if we had a warning mechanism we could issue a warning for NullableUnknown
      }

    lazy val parameterAlignmentErrors =
      parameterMisalignments ++ parameterTypeErrors

    lazy val columnAlignmentErrors =
      columnMisalignments ++ columnTypeErrors ++ columnTypeWarnings ++ nullabilityMisalignments

    lazy val alignmentErrors =
      (parameterAlignmentErrors).sortBy(m => (m.index, m.msg)) ++
      (columnAlignmentErrors).sortBy(m => (m.index, m.msg))

    /** Description of each parameter, paired with its errors. */
    lazy val paramDescriptions: List[(String, List[AlignmentError])] = {
      val params: Block =
        parameterAlignment_.zipWithIndex.map {
          case (Ior.Both((j1, n1), ParameterMeta(j2, s2, _, _)), i)  => List(f"P${i+1}%02d", show"${typeName(j1.typeStack.last, n1)}", " → ", j2.show.toUpperCase, show"($s2)")
          case (Ior.Left((j1, n1)),                              i)  => List(f"P${i+1}%02d", show"${typeName(j1.typeStack.last, n1)}", " → ", "", "")
          case (Ior.Right(          ParameterMeta(j2, s2, _, _)), i) => List(f"P${i+1}%02d", "",                     " → ", j2.show.toUpperCase, show"($s2)")
        } .transpose.map(Block(_)).foldLeft(Block(Nil))(_ leftOf1 _).trimLeft(1)
      params.toString.linesIterator.toList.zipWithIndex.map { case (show, n) =>
        (show, parameterAlignmentErrors.filter(_.index == n + 1))
      }
    }

    /** Description of each parameter, paird with its errors. */
    lazy val columnDescriptions: List[(String, List[AlignmentError])] = {
      import pretty._
      val cols: Block =
        columnAlignment_.zipWithIndex.map {
          case (Ior.Both((j1, n1), ColumnMeta(j2, s2, n2, m)), i)  => List(f"C${i+1}%02d", m, j2.show.toUpperCase, show"(${s2.toString})", formatNullability(n2), " → ", typeName(j1.typeStack.last, n1))
          case (Ior.Left((j1, n1)),                            i)  => List(f"C${i+1}%02d", "",          "", "",                       "",                    " → ", typeName(j1.typeStack.last, n1))
          case (Ior.Right(          ColumnMeta(j2, s2, n2, m)), i) => List(f"C${i+1}%02d", m, j2.show.toUpperCase, show"(${s2.toString})", formatNullability(n2), " → ", "")
        } .transpose.map(Block(_)).foldLeft(Block(Nil))(_ leftOf1 _).trimLeft(1)
      cols.toString.linesIterator.toList.zipWithIndex.map { case (show, n) =>
        (show, columnAlignmentErrors.filter(_.index == n + 1))
      }
    }

  }

  // Some stringy helpers

  private val packagePrefix = "\\b[a-z]+\\.".r

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  private def typeName(t: Option[String], n: NullabilityKnown): String = {
    val name = packagePrefix.replaceAllIn(t.fold("«erased»")(_.toString), "")
    n match {
      case NoNulls  => name
      case Nullable => show"Option[${name}]"
    }
  }

  private def formatNullability(n: Nullability): String =
    n match {
      case NoNulls         => "NOT NULL"
      case Nullable        => "NULL"
      case NullableUnknown => "NULL?"
    }

  private val MySQLDriverName = "MySQL Connector/J"

  // tweaks to the types returned by JDBC to improve analysis
  private def tweakMetaJdbcType(driver: String, jdbcType: JdbcType, vendorTypeName: String) = jdbcType match {
    // the Postgres driver does not return *WithTimezone JDBC types for *tz column types
    // https://github.com/pgjdbc/pgjdbc/issues/2485
    // https://github.com/pgjdbc/pgjdbc/issues/1766
    case JdbcType.Time if vendorTypeName.compareToIgnoreCase("timetz") == 0 => JdbcType.TimeWithTimezone
    case JdbcType.Timestamp if vendorTypeName.compareToIgnoreCase("timestamptz") == 0 => JdbcType.TimestampWithTimezone

    // MySQL timestamp columns are returned as Timestamp
    case JdbcType.Timestamp
      if vendorTypeName.compareToIgnoreCase("timestamp") == 0 && driver == MySQLDriverName => JdbcType.TimestampWithTimezone

    case t => t
  }
}

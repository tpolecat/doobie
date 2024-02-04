// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import doobie.util.log.{Success, ProcessingFailure}

trait LogSuitePlatform { self: LogSuite =>
  import doobie.generic.auto._

  test("[Query] n-arg success") {
    val Sql = "select 1 where ? = ?"
    val Arg = 1 *: 1 *: EmptyTuple
    eventForUniqueQuery(Sql, Arg) match {
      case Success(Sql, List(1, 1), _, _, _) => ()
      case a                                 => fail(s"no match: $a")
    }
  }

  test("[Query] n-arg processing failure") {
    val Sql = "select 1 where ? = ?"
    val Arg = 1 *: 2 *: EmptyTuple
    eventForUniqueQuery(Sql, Arg) match {
      case ProcessingFailure(Sql, List(1, 2), _, _, _, _) => ()
      case a                                              => fail(s"no match: $a")
    }
  }

  test("[Update] n-arg success") {
    val Sql = "update foo set bar = ?"
    val Arg = 42 *: EmptyTuple
    eventForUniqueUpdate(Sql, Arg) match {
      case Success(Sql, List(42), _, _, _) => ()
      case a                               => fail(s"no match: $a")
    }
  }

}

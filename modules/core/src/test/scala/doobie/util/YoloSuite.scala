// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import cats.effect.IO
import doobie.util.yolo._

class YoloSuite extends munit.FunSuite {

  // Kind of a bogus test; just checking for compilation
  test("YOLO checks should compile for Query, Query0, Update, Update0") {
    lazy val _ = {
      val y = new Yolo[IO](null); import y._
      (null: Query0[Int]).check
      (null: Query[Int, Int]).check
      Update0("", None).check
      Update[Int]("", None).check
    }
  }

}

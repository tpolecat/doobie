// Copyright (c) 2013-2018 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package util

import cats.effect.IO
import doobie.util.yolo._

import org.specs2.mutable.Specification

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Equals"))
object yolospec extends Specification {

  // Kind of a bogus test; just checking for compilation
  "YOLO checks" should {
    "compile for Query, Query0, Update, Update0" in {
      val dontRun = () => {
        val y = new Yolo[IO](null); import y._
        (null : Query0[Int]).check
        (null : Query[Int, Int]).check
        Update0("", None).check
        Update[Int]("", None).check
      }
      dontRun != null
    }
  }

}

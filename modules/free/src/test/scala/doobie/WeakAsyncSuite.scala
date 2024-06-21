// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie

import cats.effect.IO
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite

import scala.concurrent.duration._

class WeakAsyncSuite extends CatsEffectSuite {

  test("WeakAsync#liftK propagates cancelation") {
    val test = WeakAsync.liftK[IO, IO].use { fk =>
      fk(IO.never).timeoutTo(1.second, IO.unit)
    }
    TestControl.executeEmbed(test)
  }

}

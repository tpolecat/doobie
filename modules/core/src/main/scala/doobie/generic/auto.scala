// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.generic

import doobie.util.meta.Meta
import doobie.util.{Get, Put, Read, Write}

trait AutoDerivation
    extends Get.Auto
    with Put.Auto
    with Read.Auto
    with Write.Auto

object auto extends AutoDerivation {

  // re-export these instances so `Meta` takes priority, must be in the object
  implicit def metaProjectionGet[A](implicit m: Meta[A]): Get[A] = Get.metaProjection
  implicit def metaProjectionPut[A](implicit m: Meta[A]): Put[A] = Put.metaProjectionWrite
  implicit def fromGetRead[A](implicit G: Get[A]): Read[A] = Read.fromGet
  implicit def fromPutWrite[A](implicit P: Put[A]): Write[A] = Write.fromPut
}

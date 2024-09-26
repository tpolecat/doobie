// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie
package refined

import doobie.util.{Read, Write}
import org.tpolecat.typename.*
import doobie.util.invariant.*
import eu.timepit.refined.api.{RefType, Validate}

trait Instances {

  implicit def refinedMeta[T, P, F[_, _]](
      implicit
      metaT: Meta[T],
      validate: Validate[T, P],
      refType: RefType[F],
      manifest: TypeName[F[T, P]]
  ): Meta[F[T, P]] =
    metaT.timap[F[T, P]](
      refineType[T, P, F])(
      unwrapRefinedType[T, P, F]
    )

  implicit def refinedWrite[T, P, F[_, _]](
      implicit
      writeT: Write[T],
      refType: RefType[F]
  ): Write[F[T, P]] =
    writeT.contramap[F[T, P]](unwrapRefinedType[T, P, F])

  implicit def refinedRead[T, P, F[_, _]](
      implicit
      readT: Read[T],
      validate: Validate[T, P],
      refType: RefType[F],
      manifest: TypeName[F[T, P]]
  ): Read[F[T, P]] =
    readT.map[F[T, P]](refineType[T, P, F])

  private def refineType[T, P, F[_, _]](t: T)(
      implicit
      refType: RefType[F],
      validate: Validate[T, P],
      manifest: TypeName[F[T, P]]
  ): F[T, P] =
    rightOrException[F[T, P]](refType.refine[P](t)(validate))

  private def unwrapRefinedType[T, P, F[_, _]](ftp: F[T, P])(
      implicit refType: RefType[F]
  ): T =
    refType.unwrap(ftp)

  private def rightOrException[T](either: Either[String, T])(
      implicit manifest: TypeName[T]
  ): T =
    either match {
      case Left(err) => throw new SecondaryValidationFailed[T](err)(manifest)
      case Right(t)  => t
    }

}

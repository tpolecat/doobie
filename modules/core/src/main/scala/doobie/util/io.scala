// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

import java.io.{Console => _, _}

import cats.syntax.all._
import cats.effect.kernel.syntax.monadCancel._
import doobie.WeakAsync

/** Module for a constructor of modules of IO operations for effectful monads. */
object io {

  /** Constructor for a module of IO operations in some effectful monad. This is by no means complete; contributions
    * welcome. The construtors here expose naked lifetime-managed objects and should be used with caution; they are
    * mostly intended for library authors who wish to integrate vendor- specific behavior that relies on JDK IO.
    */
  class IOActions[M[_]](
      implicit M: WeakAsync[M]
  ) {

    private def delay[A](a: => A): M[A] = M.delay(a)

    /** Print to `Console.out`
      * @group Console Operations
      */
    def putStr(s: String): M[Unit] =
      delay(Console.out.print(s))

    /** Print to `Console.out`
      * @group Console Operations
      */
    def putStrLn(s: String): M[Unit] =
      delay(Console.out.println(s))

    /** Copy a block from `is` to `os` using naked buffer `buf`, which will be clobbered.
      * @group Stream Operations
      */
    def copyBlock(buf: Array[Byte])(is: InputStream, os: OutputStream): M[Int] =
      delay(is.read(buf)) flatMap { n => delay(os.write(buf, 0, n)).whenA(n >= 0).as(n) }

    /** Copy the contents of `file` to a `os` in blocks of size `bufSize`.
      * @group File Operations
      */
    def copyFileToStream(bufSize: Int, file: File, os: OutputStream): M[Unit] =
      withFileInputStream(file)(copyStream(new Array[Byte](bufSize))(_, os))

    /** Copy the remainder of `is` to `file` in blocks of size `bufSize`.
      * @group File Operations
      */
    def copyStreamToFile(bufSize: Int, file: File, is: InputStream): M[Unit] =
      withFileOutputStream(file)(copyStream(new Array[Byte](bufSize))(is, _))

    /** Copy the remainder of `is` into `os` using naked buffer `buf`, which will be clobbered.
      * @group Stream Operations
      */
    def copyStream(buf: Array[Byte])(is: InputStream, os: OutputStream): M[Unit] =
      copyBlock(buf)(is, os).iterateUntil(_ < 0).void

    /** Perform an operation with a `FileInputStream`, which will be closed afterward.
      * @group File Operations
      */
    def withFileInputStream[A](file: File)(f: FileInputStream => M[A]): M[A] =
      delay(new FileInputStream(file)).bracket(f)(i => delay(i.close()))

    /** Perform an operation with a `FileOutputStream`, which will be closed afterward.
      * @group File Operations
      */
    def withFileOutputStream[A](file: File)(f: FileOutputStream => M[A]): M[A] =
      delay(new FileOutputStream(file)).bracket(f)(i => delay(i.close()))

    /** Flush `os`.
      * @group Stream Operations
      */
    def flush(os: OutputStream): M[Unit] =
      delay(os.flush())

  }

}

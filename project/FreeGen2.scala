import sbt.*
import Keys.*

import java.lang.reflect.*
import scala.reflect.ClassTag
import Predef.*
import org.typelevel.sbt.gha.GenerativeKeys.githubWorkflowBuild
import org.typelevel.sbt.gha.GenerativePlugin.autoImport.WorkflowStep

object FreeGen2 {

  lazy val freeGen2Classes = settingKey[List[Class[_]]]("classes for which free algebras should be generated")
  lazy val freeGen2Dir = settingKey[File]("directory where free algebras go")
  lazy val freeGen2Package = settingKey[String]("package where free algebras go")
  lazy val freeGen2Renames = settingKey[Map[Class[_], String]]("map of imports that must be renamed")
  lazy val freeGen2AllImportExcludes =
    settingKey[Set[Class[_]]]("Imports to exclude for the generator for all generaated files")
  lazy val freeGen2KleisliInterpreterImportExcludes = settingKey[Set[Class[_]]](
    "Imports to exclude for the generator kleisliinterpreter.scala file (to avoid unused import warning) ")
  lazy val freeGen2 = taskKey[Seq[File]]("generate free algebras")

  lazy val freeGen2Settings = Seq(
    freeGen2Classes := Nil,
    freeGen2Dir := (Compile / sourceManaged).value,
    freeGen2Package := "doobie.free",
    freeGen2Renames := Map(classOf[java.sql.Array] -> "SqlArray"),
    freeGen2KleisliInterpreterImportExcludes := Set.empty,
    freeGen2AllImportExcludes := Set.empty,
    freeGen2 :=
      new FreeGen2(
        freeGen2Classes.value,
        freeGen2Package.value,
        freeGen2Renames.value,
        freeGen2AllImportExcludes.value,
        freeGen2KleisliInterpreterImportExcludes.value,
        state.value.log
      ).gen(freeGen2Dir.value)
  )

}

class FreeGen2(
    managed: List[Class[_]],
    pkg: String,
    renames: Map[Class[_], String],
    allImportExcludes: Set[Class[_]],
    kleisliImportExcludes: Set[Class[_]],
    log: Logger
) {

  // These Java classes will have non-Java names in our generated code
  val ClassBoolean = classOf[Boolean]
  val ClassByte = classOf[Byte]
  val ClassShort = classOf[Short]
  val ClassInt = classOf[Int]
  val ClassLong = classOf[Long]
  val ClassFloat = classOf[Float]
  val ClassDouble = classOf[Double]
  val ClassObject = classOf[Object]
  val ClassChar = classOf[Char]
  val ClassVoid = Void.TYPE

  def tparams(t: Type): List[String] =
    t match {
      case t: GenericArrayType  => tparams(t.getGenericComponentType)
      case t: ParameterizedType => t.getActualTypeArguments.toList.flatMap(tparams)
      case t: TypeVariable[_]   => List(t.toString)
      case _                    => Nil
    }

  def toScalaType(t: Type): String =
    t match {
      case t: GenericArrayType => s"Array[${toScalaType(t.getGenericComponentType)}]"
      case t: ParameterizedType =>
        s"${toScalaType(t.getRawType)}${t.getActualTypeArguments.map(toScalaType).mkString("[", ", ", "]")}"
      case t: WildcardType =>
        t.getUpperBounds.toList.filterNot(_ == classOf[Object]) match {
          case (c: Class[_]) :: Nil => s"? <: ${c.getName}"
          case Nil                  => "?"
          case cs                   => sys.error("unhandled upper bounds: " + cs.toList)
        }
      case t: TypeVariable[_] => t.toString
      case ClassVoid          => "Unit"
      case ClassBoolean       => "Boolean"
      case ClassByte          => "Byte"
      case ClassShort         => "Short"
      case ClassInt           => "Int"
      case ClassLong          => "Long"
      case ClassFloat         => "Float"
      case ClassDouble        => "Double"
      case ClassObject        => "AnyRef"
      case ClassChar          => "Char"
      case x: Class[_] =>
        if (x.isArray) {
          s"Array[${toScalaType(x.getComponentType)}]"
        } else if (x.getName == "java.util.Map") {
          x.getName
        } else {
          renames.getOrElse(x, x.getSimpleName)
        }
    }

  // Each constructor for our algebra maps to an underlying method, and an index is provided to
  // disambiguate in cases of overloading.
  case class Ctor(method: Method, index: Int) {

    // The method name, unchanged
    def mname: String =
      method.getName

    // The case class constructor name, capitalized and with an index when needed
    def cname: String = {
      val s = mname(0).toUpper +: mname.drop(1)
      (if (index == 0) s else s"$s$index")
    }

    // Constructor parameter type names
    def cparams: List[String] =
      method.getGenericParameterTypes.toList.map(toScalaType)

    def ctparams: String = {
      val ss = (method.getGenericParameterTypes.toList.flatMap(tparams) ++ tparams(method.getGenericReturnType)).toSet
      if (ss.isEmpty) "" else ss.mkString("[", ", ", "]")
    }

    // Constructor arguments, a .. z zipped with the right type
    def cargs: List[String] =
      "abcdefghijklmnopqrstuvwxyz".toList.zip(cparams).map {
        case (n, t) => s"$n: $t"
      }

    // Return type name
    def ret: String = toScalaType(method.getGenericReturnType)

    // Case class/object declaration
    def ctor(opname: String): String =
      ((cparams match {
        case Nil => s"|case object $cname"
        case ps  => s"|final case class $cname$ctparams(${cargs.mkString(", ")})"
      }) +
        s""" extends ${opname}[$ret] {
           |      def visit[F[_]](v: Visitor[F]) = v.$mname${if (args.isEmpty) "" else s"($args)"}
           |    }""").trim.stripMargin

    // Argument list: a, b, c, ... up to the proper arity
    def args: String =
      "abcdefghijklmnopqrstuvwxyz".toList.take(cparams.length).mkString(", ")

    // Pattern to match the constructor
    def pat: String =
      cparams match {
        case Nil => s"object $cname"
        case ps  => s"class  $cname(${cargs.mkString(", ")})"
      }

    // Case clause mapping this constructor to the corresponding primitive action
    def prim(sname: String): String =
      if (cargs.isEmpty)
        s"case $cname => primitive(_.$mname)"
      else
        s"case $cname($args) => primitive(_.$mname($args))"

    // Smart constructor
    def lifted(ioname: String): String =
      if (cargs.isEmpty) {
        s"val $mname: ${ioname}[$ret] = FF.liftF(${cname})"
      } else {
        s"def $mname$ctparams(${cargs.mkString(", ")}): ${ioname}[$ret] = FF.liftF(${cname}($args))"
      }

    def visitor: String =
      if (cargs.isEmpty)
        s"|      def $mname: F[$ret]"
      else
        s"|      def $mname$ctparams(${cargs.mkString(", ")}): F[$ret]"

    def stub: String =
      if (cargs.isEmpty)
        s"""|      def $mname: F[$ret] = sys.error("Not implemented: $mname")"""
      else {
        val cargsStr = cargs.mkString(", ")
        val cparamsStr = cparams.mkString(", ")
        s"""|      def $mname$ctparams($cargsStr): F[$ret] = sys.error("Not implemented: $mname$ctparams($cparamsStr)")"""
      }

    def kleisliImpl(oname: String): String =
      if (cargs.isEmpty)
        s"|    override def $mname: Kleisli[M, $oname, $ret] = primitive(_.$mname)"
      else
        s"|    override def $mname$ctparams(${cargs.mkString(", ")}) = primitive(_.$mname($args))"
  }

  // This class, plus any superclasses and interfaces, "all the way up"
  def closure(c: Class[_]): List[Class[_]] =
    (c :: (Option(c.getSuperclass).toList ++ c.getInterfaces.toList).flatMap(closure)).distinct
      .filterNot(_.getName == "java.lang.AutoCloseable") // not available in jdk1.6
      .filterNot(_.getName == "java.lang.Object") // we don't want .equals, etc.

  implicit class MethodOps(m: Method) {
    def isStatic: Boolean =
      (m.getModifiers & Modifier.STATIC) != 0
  }

  // All non-deprecated methods for this class and any superclasses/interfaces
  def methods(c: Class[_]): List[Method] =
    closure(c).flatMap(_.getDeclaredMethods.toList).distinct
      .filterNot(_.isStatic)
      .filter(_.getAnnotation(classOf[Deprecated]) == null)

  // Ctor values for all methods in of A plus superclasses, interfaces, etc.
  def ctors[A](implicit ev: ClassTag[A]): List[Ctor] =
    methods(ev.runtimeClass).groupBy(_.getName).toList.flatMap { case (n, ms) =>
      ms.sortBy(_.getGenericParameterTypes.map(toScalaType).mkString(",")).zipWithIndex.map {
        case (m, i) => Ctor(m, i)
      }
    }.sortBy(c => (c.mname, c.index))

  // Fully qualified rename, if any
  def renameImport(c: Class[_]): String = {
    val origName = c.getSimpleName
    renames.get(c) match {
      case None          => s"import ${c.getName}"
      case Some(renamed) => s"import ${c.getPackage.getName}.{ $origName as $renamed }"
    }
  }

  import scala.util.chaining._

  // All types referenced by all methods on A, superclasses, interfaces, etc.
  def imports[A](excludeImports: Set[Class[_]])(implicit ev: ClassTag[A]): List[String] = {
    (renameImport(ev.runtimeClass) :: ctors.map(_.method).flatMap { m =>
      m.getReturnType :: m.getParameterTypes.toList
    }.map { t =>
      if (t.isArray) t.getComponentType else t
    }.filterNot(t => t.isPrimitive || t == classOf[Object] || excludeImports.contains(t)).map { c =>
      renameImport(c)
    }).distinct.sorted
  }

  // The algebra module for A
  def module[A](implicit ev: ClassTag[A]): String = {
    val oname = ev.runtimeClass.getSimpleName // original name, without name mapping
    val sname = toScalaType(ev.runtimeClass)
    val opname = s"${oname}Op"
    val ioname = s"${oname}IO"
    val mname = oname.toLowerCase
    s"""
    |
    |// format: off
    |
    |package $pkg
    |
    |import cats.{~>, Applicative, Semigroup, Monoid}
    |import cats.effect.kernel.{ CancelScope, Poll, Sync }
    |import cats.free.{ Free as FF } // alias because some algebras have an op called Free
    |import doobie.util.log.LogEvent
    |import doobie.WeakAsync
    |import scala.concurrent.Future
    |import scala.concurrent.duration.FiniteDuration
    |
    |${imports[A](excludeImports = allImportExcludes).mkString("\n")}
    |
    |// This file is Auto-generated using FreeGen2.scala
    |object $mname { module =>
    |
    |  // Algebra of operations for $sname. Each accepts a visitor as an alternative to pattern-matching.
    |  sealed trait ${opname}[A] {
    |    def visit[F[_]](v: ${opname}.Visitor[F]): F[A]
    |  }
    |
    |  // Free monad over ${opname}.
    |  type ${ioname}[A] = FF[${opname}, A]
    |
    |  // Module of instances and constructors of ${opname}.
    |  object ${opname} {
    |
    |    // Given a $sname we can embed a ${ioname} program in any algebra that understands embedding.
    |    implicit val ${opname}Embeddable: Embeddable[${opname}, ${sname}] =
    |      new Embeddable[${opname}, ${sname}] {
    |        def embed[A](j: ${sname}, fa: FF[${opname}, A]): Embedded.$oname[A] = Embedded.$oname(j, fa)
    |      }
    |
    |    // Interface for a natural transformation ${opname} ~> F encoded via the visitor pattern.
    |    // This approach is much more efficient than pattern-matching for large algebras.
    |    trait Visitor[F[_]] extends (${opname} ~> F) {
    |      final def apply[A](fa: ${opname}[A]): F[A] = fa.visit(this)
    |
    |      // Common
    |      def raw[A](f: $sname => A): F[A]
    |      def embed[A](e: Embedded[A]): F[A]
    |      def raiseError[A](e: Throwable): F[A]
    |      def handleErrorWith[A](fa: ${ioname}[A])(f: Throwable => ${ioname}[A]): F[A]
    |      def monotonic: F[FiniteDuration]
    |      def realTime: F[FiniteDuration]
    |      def delay[A](thunk: => A): F[A]
    |      def suspend[A](hint: Sync.Type)(thunk: => A): F[A]
    |      def forceR[A, B](fa: ${ioname}[A])(fb: ${ioname}[B]): F[B]
    |      def uncancelable[A](body: Poll[${ioname}] => ${ioname}[A]): F[A]
    |      def poll[A](poll: Any, fa: ${ioname}[A]): F[A]
    |      def canceled: F[Unit]
    |      def onCancel[A](fa: ${ioname}[A], fin: ${ioname}[Unit]): F[A]
    |      def fromFuture[A](fut: ${ioname}[Future[A]]): F[A]
    |      def fromFutureCancelable[A](fut: ${ioname}[(Future[A], ${ioname}[Unit])]): F[A]
    |      def cancelable[A](fa: ${ioname}[A], fin: ${ioname}[Unit]): F[A]
    |      def performLogging(event: LogEvent): F[Unit]
    |
    |      // $sname
          ${ctors[A].map(_.visitor).mkString("\n    ")}
    |
    |    }
    |
    |    // Common operations for all algebras.
    |    final case class Raw[A](f: $sname => A) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    |    }
    |    final case class Embed[A](e: Embedded[A]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    |    }
    |    final case class RaiseError[A](e: Throwable) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    |    }
    |    final case class HandleErrorWith[A](fa: ${ioname}[A], f: Throwable => ${ioname}[A]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa)(f)
    |    }
    |    case object Monotonic extends ${opname}[FiniteDuration] {
    |      def visit[F[_]](v: Visitor[F]) = v.monotonic
    |    }
    |    case object Realtime extends ${opname}[FiniteDuration] {
    |      def visit[F[_]](v: Visitor[F]) = v.realTime
    |    }
    |    case class Suspend[A](hint: Sync.Type, thunk: () => A) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.suspend(hint)(thunk())
    |    }
    |    case class ForceR[A, B](fa: ${ioname}[A], fb: ${ioname}[B]) extends ${opname}[B] {
    |      def visit[F[_]](v: Visitor[F]) = v.forceR(fa)(fb)
    |    }
    |    case class Uncancelable[A](body: Poll[${ioname}] => ${ioname}[A]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.uncancelable(body)
    |    }
    |    case class Poll1[A](poll: Any, fa: ${ioname}[A]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.poll(poll, fa)
    |    }
    |    case object Canceled extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.canceled
    |    }
    |    case class OnCancel[A](fa: ${ioname}[A], fin: ${ioname}[Unit]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.onCancel(fa, fin)
    |    }
    |    case class FromFuture[A](fut: ${ioname}[Future[A]]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.fromFuture(fut)
    |    }
    |    case class FromFutureCancelable[A](fut: ${ioname}[(Future[A], ${ioname}[Unit])]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.fromFutureCancelable(fut)
    |    }
    |    case class Cancelable[A](fa: ${ioname}[A], fin: ${ioname}[Unit]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.cancelable(fa, fin)
    |    }
    |    case class PerformLogging(event: LogEvent) extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.performLogging(event)
    |    }
    |
    |    // $sname-specific operations.
    |    ${ctors[A].map(_.ctor(opname)).mkString("\n    ")}
    |
    |  }
    |  import ${opname}.*
    |
    |  // Smart constructors for operations common to all algebras.
    |  val unit: ${ioname}[Unit] = FF.pure[${opname}, Unit](())
    |  def pure[A](a: A): ${ioname}[A] = FF.pure[${opname}, A](a)
    |  def raw[A](f: $sname => A): ${ioname}[A] = FF.liftF(Raw(f))
    |  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[${opname}, A] = FF.liftF(Embed(ev.embed(j, fa)))
    |  def raiseError[A](err: Throwable): ${ioname}[A] = FF.liftF[${opname}, A](RaiseError(err))
    |  def handleErrorWith[A](fa: ${ioname}[A])(f: Throwable => ${ioname}[A]): ${ioname}[A] = FF.liftF[${opname}, A](HandleErrorWith(fa, f))
    |  val monotonic = FF.liftF[${opname}, FiniteDuration](Monotonic)
    |  val realtime = FF.liftF[${opname}, FiniteDuration](Realtime)
    |  def delay[A](thunk: => A) = FF.liftF[${opname}, A](Suspend(Sync.Type.Delay, () => thunk))
    |  def suspend[A](hint: Sync.Type)(thunk: => A) = FF.liftF[${opname}, A](Suspend(hint, () => thunk))
    |  def forceR[A, B](fa: ${ioname}[A])(fb: ${ioname}[B]) = FF.liftF[${opname}, B](ForceR(fa, fb))
    |  def uncancelable[A](body: Poll[${ioname}] => ${ioname}[A]) = FF.liftF[${opname}, A](Uncancelable(body))
    |  def capturePoll[M[_]](mpoll: Poll[M]) = new Poll[${ioname}] {
    |    def apply[A](fa: ${ioname}[A]) = FF.liftF[${opname}, A](Poll1(mpoll, fa))
    |  }
    |  val canceled = FF.liftF[${opname}, Unit](Canceled)
    |  def onCancel[A](fa: ${ioname}[A], fin: ${ioname}[Unit]) = FF.liftF[${opname}, A](OnCancel(fa, fin))
    |  def fromFuture[A](fut: ${ioname}[Future[A]]) = FF.liftF[${opname}, A](FromFuture(fut))
    |  def fromFutureCancelable[A](fut: ${ioname}[(Future[A], ${ioname}[Unit])]) = FF.liftF[${opname}, A](FromFutureCancelable(fut))
    |  def cancelable[A](fa: ${ioname}[A], fin: ${ioname}[Unit]) = FF.liftF[${opname}, A](Cancelable(fa, fin))
    |  def performLogging(event: LogEvent) = FF.liftF[${opname}, Unit](PerformLogging(event))
    |
    |  // Smart constructors for $oname-specific operations.
    |  ${ctors[A].map(_.lifted(ioname)).mkString("\n  ")}
    |
    |  // Typeclass instances for ${ioname}
    |  implicit val WeakAsync${ioname}: WeakAsync[${ioname}] =
    |    new WeakAsync[${ioname}] {
    |      val monad = FF.catsFreeMonadForFree[${opname}]
    |      override val applicative: Applicative[$ioname] = monad
    |      override val rootCancelScope: CancelScope = CancelScope.Cancelable
    |      override def pure[A](x: A): ${ioname}[A] = monad.pure(x)
    |      override def flatMap[A, B](fa: ${ioname}[A])(f: A => ${ioname}[B]): ${ioname}[B] = monad.flatMap(fa)(f)
    |      override def tailRecM[A, B](a: A)(f: A => ${ioname}[Either[A, B]]): ${ioname}[B] = monad.tailRecM(a)(f)
    |      override def raiseError[A](e: Throwable): ${ioname}[A] = module.raiseError(e)
    |      override def handleErrorWith[A](fa: ${ioname}[A])(f: Throwable => ${ioname}[A]): ${ioname}[A] = module.handleErrorWith(fa)(f)
    |      override def monotonic: ${ioname}[FiniteDuration] = module.monotonic
    |      override def realTime: ${ioname}[FiniteDuration] = module.realtime
    |      override def suspend[A](hint: Sync.Type)(thunk: => A): ${ioname}[A] = module.suspend(hint)(thunk)
    |      override def forceR[A, B](fa: ${ioname}[A])(fb: ${ioname}[B]): ${ioname}[B] = module.forceR(fa)(fb)
    |      override def uncancelable[A](body: Poll[${ioname}] => ${ioname}[A]): ${ioname}[A] = module.uncancelable(body)
    |      override def canceled: ${ioname}[Unit] = module.canceled
    |      override def onCancel[A](fa: ${ioname}[A], fin: ${ioname}[Unit]): ${ioname}[A] = module.onCancel(fa, fin)
    |      override def fromFuture[A](fut: ${ioname}[Future[A]]): ${ioname}[A] = module.fromFuture(fut)
    |      override def fromFutureCancelable[A](fut: ${ioname}[(Future[A], ${ioname}[Unit])]): ${ioname}[A] = module.fromFutureCancelable(fut)
    |      override def cancelable[A](fa: ${ioname}[A], fin: ${ioname}[Unit]): ${ioname}[A] = module.cancelable(fa, fin)
    |    }
    |    
    |  implicit def Monoid$ioname[A : Monoid]: Monoid[$ioname[A]] = new Monoid[$ioname[A]] {
    |    override def empty: $ioname[A] = Applicative[$ioname].pure(Monoid[A].empty)
    |    override def combine(x: $ioname[A], y: $ioname[A]): $ioname[A] =
    |      Applicative[$ioname].product(x, y).map { case (x, y) => Monoid[A].combine(x, y) }
    |  }
 
    |  implicit def Semigroup$ioname[A : Semigroup]: Semigroup[$ioname[A]] = new Semigroup[$ioname[A]] {
    |    override def combine(x: $ioname[A], y: $ioname[A]): $ioname[A] =
    |      Applicative[$ioname].product(x, y).map { case (x, y) => Semigroup[A].combine(x, y) }
    |  }  
    |}
    |""".trim.stripMargin
  }

  def embed[A](implicit ev: ClassTag[A]): String = {
    val sname = ev.runtimeClass.getSimpleName
    s"final case class $sname[A](j: ${ev.runtimeClass.getName}, fa: ${sname}IO[A]) extends Embedded[A]"
  }

  // Import for the IO type for a carrer type, with renaming
  def ioImport(c: Class[_]): String = {
    val sn = c.getSimpleName
    s"import ${sn.toLowerCase}.${sn}IO"
  }

  // The Embedded definition for all modules.
  def embeds: String =
    s"""
       |
       |// format: off
       |
       |package $pkg
       |
       |import cats.free.Free
       |
       |${managed.map(ioImport).mkString("\n")}
       |
       |// A pair (J, Free[F, A]) with constructors that tie down J and F.
       |sealed trait Embedded[A]
       |
       |object Embedded {
       |  ${managed.map(ClassTag(_)).map(embed(_)).mkString("\n  ")}
       |}
       |
       |// Typeclass for embeddable pairs (J, F)
       |trait Embeddable[F[_], J] {
       |  def embed[A](j: J, fa: Free[F, A]): Embedded[A]
       |}
       |""".trim.stripMargin

  def interp[A](implicit ev: ClassTag[A]): String = {
    val oname = ev.runtimeClass.getSimpleName // original name, without name mapping
    val sname = toScalaType(ev.runtimeClass)
    val opname = s"${oname}Op"
    val ioname = s"${oname}IO"

    def kles(retType: String) = s"Kleisli[M, $sname, $retType]"

    val klesA = kles("A")
    val klesUnit = kles("Unit")
    val mname = oname.toLowerCase
    s"""
       |  trait ${oname}Interpreter extends ${oname}Op.Visitor[Kleisli[M, $sname, *]] {
       |
       |    // common operations delegate to outer interpreter
       |    override def raw[A](f: ${sname} => A): $klesA = outer.raw(f)
       |    override def embed[A](e: Embedded[A]): $klesA = outer.embed(e)
       |    override def raiseError[A](e: Throwable): $klesA = outer.raiseError(e)
       |    override def monotonic: ${kles("FiniteDuration")} = outer.monotonic[${sname}]
       |    override def realTime: ${kles("FiniteDuration")} = outer.realTime[${sname}]
       |    override def delay[A](thunk: => A): $klesA = outer.delay(thunk)
       |    override def suspend[A](hint: Sync.Type)(thunk: => A): $klesA = outer.suspend(hint)(thunk)
       |    override def canceled: $klesUnit = outer.canceled[${sname}]
       |
       |    override def performLogging(event: LogEvent): $klesUnit = Kleisli(_ => logHandler.run(event))
       |
       |    // for operations using ${ioname} we must call ourself recursively
       |    override def handleErrorWith[A](fa: ${ioname}[A])(f: Throwable => ${ioname}[A]): $klesA = outer.handleErrorWith(this)(fa)(f)
       |    override def forceR[A, B](fa: ${ioname}[A])(fb: ${ioname}[B]): ${kles("B")} = outer.forceR(this)(fa)(fb)
       |    override def uncancelable[A](body: Poll[${ioname}] => ${ioname}[A]): $klesA = outer.uncancelable(this, ${pkg}.${mname}.capturePoll)(body)
       |    override def poll[A](poll: Any, fa: ${ioname}[A]): $klesA = outer.poll(this)(poll, fa)
       |    override def onCancel[A](fa: ${ioname}[A], fin: ${ioname}[Unit]): $klesA = outer.onCancel(this)(fa, fin)
       |    override def fromFuture[A](fut: ${ioname}[Future[A]]): $klesA = outer.fromFuture(this)(fut)
       |    override def fromFutureCancelable[A](fut: ${ioname}[(Future[A], ${ioname}[Unit])]): $klesA = outer.fromFutureCancelable(this)(fut)
       |    override def cancelable[A](fa: ${ioname}[A], fin: ${ioname}[Unit]): $klesA = outer.cancelable(this)(fa, fin)
       |
       |
       |    // domain-specific operations are implemented in terms of `primitive`
       |${ctors[A].map(_.kleisliImpl(sname)).mkString("\n")}
       |
       |  }
       |""".trim.stripMargin
  }

  def interpreterDef(c: Class[_]): String = {
    val oname = c.getSimpleName // original name, without name mapping
    val sname = toScalaType(c)
    val opname = s"${oname}Op"
    val ioname = s"${oname}IO"
    val mname = oname.toLowerCase
    s"lazy val ${oname}Interpreter: ${opname} ~> Kleisli[M, $sname, *] = new ${oname}Interpreter { }"
  }

  // template for a kleisli interpreter
  def kleisliInterpreter: String = {
    val extraImports =
      managed.map(ClassTag(_))
        .flatMap { c => imports(kleisliImportExcludes ++ allImportExcludes)(c) }
        .distinct.sorted.mkString("\n")

    val kleisliInterpreterEmbedMatch =
      managed.map(_.getSimpleName)
        .map { n => s"case Embedded.${n}(j, fa) => Kleisli(_ => fa.foldMap(${n}Interpreter).run(j))" }
        .mkString("\n      ")

    s"""
       |
       |// format: off
       |
       |package $pkg
       |
       |// Library imports
       |import cats.~>
       |import cats.data.Kleisli
       |import cats.effect.kernel.{ Poll, Sync }
       |import cats.free.Free
       |import doobie.WeakAsync
       |import doobie.util.log.{LogEvent, LogHandler}
       |import scala.concurrent.Future
       |import scala.concurrent.duration.FiniteDuration
       |
       |// Types referenced in the JDBC API
       |$extraImports
       |
       |// Algebras and free monads thereof referenced by our interpreter.
       |${managed.map(_.getSimpleName).map(c => s"import ${pkg}.${c.toLowerCase}.{ ${c}IO, ${c}Op }").mkString("\n")}
       |
       |object KleisliInterpreter {
       |  def apply[M[_]: WeakAsync](logHandler: LogHandler[M]): KleisliInterpreter[M] =
       |    new KleisliInterpreter[M](logHandler)
       |}
       |
       |// Family of interpreters into Kleisli arrows for some monad M.
       |class KleisliInterpreter[M[_]](logHandler: LogHandler[M])(implicit val asyncM: WeakAsync[M]) { outer =>
       |
       |  // The ${managed.length} interpreters, with definitions below. These can be overridden to customize behavior.
       |  ${managed.map(interpreterDef).mkString("\n  ")}
       |
       |  // Some methods are common to all interpreters and can be overridden to change behavior globally.
       |  def primitive[J, A](f: J => A): Kleisli[M, J, A] = Kleisli { a =>
       |    // primitive JDBC methods throw exceptions and so do we when reading values
       |    // so catch any non-fatal exceptions and lift them into the effect
       |    try {
       |      asyncM.blocking(f(a))
       |    } catch {
       |      case scala.util.control.NonFatal(e) => asyncM.raiseError(e)
       |    }
       |  }
       |  def raw[J, A](f: J => A): Kleisli[M, J, A] = primitive(f)
       |  def raiseError[J, A](e: Throwable): Kleisli[M, J, A] = Kleisli(_ => asyncM.raiseError(e))
       |  def monotonic[J]: Kleisli[M, J, FiniteDuration] = Kleisli(_ => asyncM.monotonic)
       |  def realTime[J]: Kleisli[M, J, FiniteDuration] = Kleisli(_ => asyncM.realTime)
       |  def delay[J, A](thunk: => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.delay(thunk))
       |  def suspend[J, A](hint: Sync.Type)(thunk: => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.suspend(hint)(thunk))
       |  def canceled[J]: Kleisli[M, J, Unit] = Kleisli(_ => asyncM.canceled)
       |
       |  // for operations using free structures we call the interpreter recursively
       |  def handleErrorWith[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A])(f: Throwable => Free[G, A]): Kleisli[M, J, A] = Kleisli (j =>
       |    asyncM.handleErrorWith(fa.foldMap(interpreter).run(j))(f.andThen(_.foldMap(interpreter).run(j)))
       |  )
       |  def forceR[G[_], J, A, B](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A])(fb: Free[G, B]): Kleisli[M, J, B] = Kleisli (j =>
       |    asyncM.forceR(fa.foldMap(interpreter).run(j))(fb.foldMap(interpreter).run(j))
       |  )
       |  def uncancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *], capture: Poll[M] => Poll[Free[G, *]])(body: Poll[Free[G, *]] => Free[G, A]): Kleisli[M, J, A] = Kleisli(j =>  
       |    asyncM.uncancelable(body.compose(capture).andThen(_.foldMap(interpreter).run(j)))
       |  )
       |  def poll[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(mpoll: Any, fa: Free[G, A]): Kleisli[M, J, A] = Kleisli(j => 
       |    mpoll.asInstanceOf[Poll[M]].apply(fa.foldMap(interpreter).run(j))
       |  )
       |  def onCancel[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A], fin: Free[G, Unit]): Kleisli[M, J, A] = Kleisli (j =>
       |    asyncM.onCancel(fa.foldMap(interpreter).run(j), fin.foldMap(interpreter).run(j))
       |  )
       |  def fromFuture[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fut: Free[G, Future[A]]): Kleisli[M, J, A] = Kleisli(j =>
       |    asyncM.fromFuture(fut.foldMap(interpreter).run(j))
       |  )
       |  def fromFutureCancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fut: Free[G, (Future[A], Free[G, Unit])]): Kleisli[M, J, A] = Kleisli(j =>
       |    asyncM.fromFutureCancelable(fut.map { case (f, g) => (f, g.foldMap(interpreter).run(j)) }.foldMap(interpreter).run(j))
       |  )
       |  def cancelable[G[_], J, A](interpreter: G ~> Kleisli[M, J, *])(fa: Free[G, A], fin: Free[G, Unit]): Kleisli[M, J, A] = Kleisli (j =>
       |    asyncM.cancelable(fa.foldMap(interpreter).run(j), fin.foldMap(interpreter).run(j))
       |  )
       |  def embed[J, A](e: Embedded[A]): Kleisli[M, J, A] =
       |    e match {
       |      $kleisliInterpreterEmbedMatch
       |    }
       |
       |  // Interpreters
       |${managed.map(ClassTag(_)).map(interp(_)).mkString("\n")}
       |
       |}
       |""".trim.stripMargin
  }

  def gen(base: File): Seq[java.io.File] = {
    import java.io._
    log.info("Generating free algebras into " + base)
    val fs = managed.map { c =>
      base.mkdirs
      val mod = module(ClassTag(c))
      val file = new File(base, s"${c.getSimpleName.toLowerCase}.scala")
      val pw = new PrintWriter(file)
      pw.println(mod)
      pw.close()
      log.info(s"${c.getName} -> ${file.getName}")
      file
    }
    val e = {
      val file = new File(base, s"embedded.scala")
      val pw = new PrintWriter(file)
      pw.println(embeds)
      pw.close()
      log.info(s"... -> ${file.getName}")
      file
    }
    val ki = {
      val file = new File(base, s"kleisliinterpreter.scala")
      val pw = new PrintWriter(file)
      pw.println(kleisliInterpreter)
      pw.close()
      log.info(s"... -> ${file.getName}")
      file
    }
    ki :: e :: fs
  }

}

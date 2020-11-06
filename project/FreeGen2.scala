import sbt._, Keys._
import java.lang.reflect._
import scala.reflect.ClassTag
import Predef._

object FreeGen2 {

  lazy val freeGen2Classes = settingKey[List[Class[_]]]("classes for which free algebras should be generated")
  lazy val freeGen2Dir     = settingKey[File]("directory where free algebras go")
  lazy val freeGen2Package = settingKey[String]("package where free algebras go")
  lazy val freeGen2Renames = settingKey[Map[Class[_], String]]("map of imports that must be renamed")
  lazy val freeGen2        = taskKey[Seq[File]]("generate free algebras")

  lazy val freeGen2Settings = Seq(
    freeGen2Classes := Nil,
    freeGen2Dir     := (sourceManaged in Compile).value,
    freeGen2Package := "doobie.free",
    freeGen2Renames := Map(classOf[java.sql.Array] -> "SqlArray"),
    freeGen2        :=
      new FreeGen2(
        freeGen2Classes.value,
        freeGen2Package.value,
        freeGen2Renames.value,
        state.value.log
      ).gen(freeGen2Dir.value)
  )

}

class FreeGen2(managed: List[Class[_]], pkg: String, renames: Map[Class[_], String], log: Logger) {

  // These Java classes will have non-Java names in our generated code
  val ClassBoolean  = classOf[Boolean]
  val ClassByte     = classOf[Byte]
  val ClassShort    = classOf[Short]
  val ClassInt      = classOf[Int]
  val ClassLong     = classOf[Long]
  val ClassFloat    = classOf[Float]
  val ClassDouble   = classOf[Double]
  val ClassObject   = classOf[Object]
  val ClassVoid     = Void.TYPE

  def tparams(t: Type): List[String] =
    t match {
      case t: GenericArrayType  => tparams(t.getGenericComponentType)
      case t: ParameterizedType => t.getActualTypeArguments.toList.flatMap(tparams)
      case t: TypeVariable[_]   => List(t.toString)
      case _                    => Nil
    }

  def toScalaType(t: Type): String =
    t match {
      case t: GenericArrayType  => s"Array[${toScalaType(t.getGenericComponentType)}]"
      case t: ParameterizedType => s"${toScalaType(t.getRawType)}${t.getActualTypeArguments.map(toScalaType).mkString("[", ", ", "]")}"
      case t: WildcardType      =>
        t.getUpperBounds.toList.filterNot(_ == classOf[Object]) match {
          case (c: Class[_]) :: Nil => s"_ <: ${c.getName}"
          case      Nil => "_"
          case cs       => sys.error("unhandled upper bounds: " + cs.toList)
        }
      case t: TypeVariable[_]   => t.toString
      case ClassVoid            => "Unit"
      case ClassBoolean         => "Boolean"
      case ClassByte            => "Byte"
      case ClassShort           => "Short"
      case ClassInt             => "Int"
      case ClassLong            => "Long"
      case ClassFloat           => "Float"
      case ClassDouble          => "Double"
      case ClassObject          => "AnyRef"
      case x: Class[_] if x.isArray => s"Array[${toScalaType(x.getComponentType)}]"
      case x: Class[_]          => renames.getOrElse(x, x.getSimpleName)
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
    def ret: String =
      toScalaType(method.getGenericReturnType)


    // Case class/object declaration
    def ctor(opname:String): String =
      ("|final case " + (cparams match {
        case Nil => s"object $cname"
        case ps  => s"class  $cname$ctparams(${cargs.mkString(", ")})"
      }) + s""" extends ${opname}[$ret] {
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
    def prim(sname:String): String =
      (if (cargs.isEmpty)
        s"case $cname => primitive(_.$mname)"
      else
        s"case $cname($args) => primitive(_.$mname($args))")

    // Smart constructor
    def lifted(ioname: String): String =
      if (cargs.isEmpty) {
        s"val $mname: ${ioname}[$ret] = FF.liftF(${cname})"
      } else {
        s"def $mname$ctparams(${cargs.mkString(", ")}): ${ioname}[$ret] = FF.liftF(${cname}($args))"
      }

    def visitor: String =
      if (cargs.isEmpty) s"|      def $mname: F[$ret]"
      else s"|      def $mname$ctparams(${cargs.mkString(", ")}): F[$ret]"

    def stub: String =
      if (cargs.isEmpty) s"""|      def $mname: F[$ret] = sys.error("Not implemented: $mname")"""
      else s"""|      def $mname$ctparams(${cargs.mkString(", ")}): F[$ret] = sys.error("Not implemented: $mname$ctparams(${cparams.mkString(", ")})")"""

    def kleisliImpl: String =
      if (cargs.isEmpty) s"""|    override def $mname = primitive(_.$mname, "$mname")"""
      else s"""|    override def $mname$ctparams(${cargs.mkString(", ")}) = primitive(_.$mname($args), "$mname", $args)"""

  }

  // This class, plus any superclasses and interfaces, "all the way up"
  def closure(c: Class[_]): List[Class[_]] =
    (c :: (Option(c.getSuperclass).toList ++ c.getInterfaces.toList).flatMap(closure)).distinct
      .filterNot(_.getName == "java.lang.AutoCloseable") // not available in jdk1.6
      .filterNot(_.getName == "java.lang.Object")        // we don't want .equals, etc.

  implicit class MethodOps(m: Method) {
    def isStatic: Boolean =
      (m.getModifiers & Modifier.STATIC) != 0
  }

  // All method for this class and any superclasses/interfaces
  def methods(c: Class[_]): List[Method] =
    closure(c).flatMap(_.getDeclaredMethods.toList).distinct.filterNot(_.isStatic)

  // Ctor values for all methods in of A plus superclasses, interfaces, etc.
  def ctors[A](implicit ev: ClassTag[A]): List[Ctor] =
    methods(ev.runtimeClass).groupBy(_.getName).toList.flatMap { case (n, ms) =>
      ms.toList.sortBy(_.getGenericParameterTypes.map(toScalaType).mkString(",")).zipWithIndex.map {
        case (m, i) => Ctor(m, i)
      }
    }.sortBy(c => (c.mname, c.index))

  // Fully qualified rename, if any
  def renameImport(c: Class[_]): String = {
    val sn = c.getSimpleName
    val an = renames.getOrElse(c, sn)
    if (sn == an) s"import ${c.getName}"
    else          s"import ${c.getPackage.getName}.{ $sn => $an }"
  }

  // All types referenced by all methods on A, superclasses, interfaces, etc.
  def imports[A](implicit ev: ClassTag[A]): List[String] =
    (renameImport(ev.runtimeClass) :: ctors.map(_.method).flatMap { m =>
      m.getReturnType :: m.getParameterTypes.toList
    }.map { t =>
      if (t.isArray) t.getComponentType else t
    }.filterNot(t => t.isPrimitive || t == classOf[Object]).map { c =>
      renameImport(c)
    }).distinct.sorted

  // The algebra module for A
  def module[A](implicit ev: ClassTag[A]): String = {
    val oname = ev.runtimeClass.getSimpleName // original name, without name mapping
    val sname = toScalaType(ev.runtimeClass)
    val opname = s"${oname}Op"
    val ioname = s"${oname}IO"
    val mname  = oname.toLowerCase
   s"""
    |package $pkg
    |
    |import cats.~>
    |import cats.effect.{ Async, ContextShift, ExitCase }
    |import cats.free.{ Free => FF } // alias because some algebras have an op called Free
    |import scala.concurrent.ExecutionContext
    |import com.github.ghik.silencer.silent
    |import io.chrisdavenport.log4cats.MessageLogger
    |
    |${imports[A].mkString("\n")}
    |
    |@silent("deprecated")
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
    |        def embed[A](j: ${sname}, fa: FF[${opname}, A]) = Embedded.${oname}(j, fa)
    |      }
    |
    |    // Interface for a natural transformation ${opname} ~> F encoded via the visitor pattern.
    |    // This approach is much more efficient than pattern-matching for large algebras.
    |    trait Visitor[F[_]] extends (${opname} ~> F) {
    |      final def apply[A](fa: ${opname}[A]): F[A] = fa.visit(this)
    |
    |      // Common
    |      def raw[A](message: => String, f: $sname => A): F[A]
    |      def embed[A](e: Embedded[A]): F[A]
    |
    |      // Async
    |      def delay[A](a: () => A): F[A]
    |      def handleErrorWith[A](fa: ${ioname}[A], f: Throwable => ${ioname}[A]): F[A]
    |      def raiseError[A](e: Throwable): F[A]
    |      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
    |      def asyncF[A](k: (Either[Throwable, A] => Unit) => ${ioname}[Unit]): F[A]
    |      def bracketCase[A, B](acquire: ${ioname}[A])(use: A => ${ioname}[B])(release: (A, ExitCase[Throwable]) => ${ioname}[Unit]): F[B]
    |      def shift: F[Unit]
    |      def evalOn[A](ec: ExecutionContext)(fa: ${ioname}[A]): F[A]
    |
    |      // Logger
    |      def error(message: => String): F[Unit]
    |      def warn(message: => String): F[Unit]
    |      def info(message: => String): F[Unit]
    |      def debug(message: => String): F[Unit]
    |      def trace(message: => String): F[Unit]
    |
    |      // $sname
          ${ctors[A].map(_.visitor).mkString("\n    ")}
    |
    |    }
    |
    |    // Common operations for all algebras.
    |    final case class Raw[A](message: () => String, f: $sname => A) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.raw(message(), f)
    |    }
    |    final case class Embed[A](e: Embedded[A]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    |    }
    |    final case class Delay[A](a: () => A) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    |    }
    |    final case class HandleErrorWith[A](fa: ${ioname}[A], f: Throwable => ${ioname}[A]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    |    }
    |    final case class RaiseError[A](e: Throwable) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.raiseError(e)
    |    }
    |    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.async(k)
    |    }
    |    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => ${ioname}[Unit]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    |    }
    |    final case class BracketCase[A, B](acquire: ${ioname}[A], use: A => ${ioname}[B], release: (A, ExitCase[Throwable]) => ${ioname}[Unit]) extends ${opname}[B] {
    |      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    |    }
    |    final case object Shift extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.shift
    |    }
    |    final case class EvalOn[A](ec: ExecutionContext, fa: ${ioname}[A]) extends ${opname}[A] {
    |      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    |    }
    |    final case class LogError(message: () => String) extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.error(message()): F[Unit]
    |    }
    |    final case class LogWarn(message: () => String) extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.warn(message()): F[Unit]
    |    }
    |    final case class LogInfo(message: () => String) extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.info(message()): F[Unit]
    |    }
    |    final case class LogDebug(message: () => String) extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.debug(message()): F[Unit]
    |    }
    |    final case class LogTrace(message: () => String) extends ${opname}[Unit] {
    |      def visit[F[_]](v: Visitor[F]) = v.trace(message()): F[Unit]
    |    }

    |    // $sname-specific operations.
    |    ${ctors[A].map(_.ctor(opname)).mkString("\n    ")}
    |
    |  }
    |  import ${opname}._
    |
    |  // Smart constructors for operations common to all algebras.
    |  val unit: ${ioname}[Unit] = FF.pure[${opname}, Unit](())
    |  def pure[A](a: A): ${ioname}[A] = FF.pure[${opname}, A](a)
    |  def raw[A](message: => String)(f: $sname => A): ${ioname}[A] = FF.liftF(Raw(() => message, f))
    |  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[${opname}, A] = FF.liftF(Embed(ev.embed(j, fa)))
    |  def delay[A](a: => A): ${ioname}[A] = FF.liftF(Delay(() => a))
    |  def handleErrorWith[A](fa: ${ioname}[A], f: Throwable => ${ioname}[A]): ${ioname}[A] = FF.liftF[${opname}, A](HandleErrorWith(fa, f))
    |  def raiseError[A](err: Throwable): ${ioname}[A] = FF.liftF[${opname}, A](RaiseError(err))
    |  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ${ioname}[A] = FF.liftF[${opname}, A](Async1(k))
    |  def asyncF[A](k: (Either[Throwable, A] => Unit) => ${ioname}[Unit]): ${ioname}[A] = FF.liftF[${opname}, A](AsyncF(k))
    |  def bracketCase[A, B](acquire: ${ioname}[A])(use: A => ${ioname}[B])(release: (A, ExitCase[Throwable]) => ${ioname}[Unit]): ${ioname}[B] = FF.liftF[${opname}, B](BracketCase(acquire, use, release))
    |  val shift: ${ioname}[Unit] = FF.liftF[${opname}, Unit](Shift)
    |  def evalOn[A](ec: ExecutionContext)(fa: ${ioname}[A]) = FF.liftF[${opname}, A](EvalOn(ec, fa))
    |
    |  // Logger
    |  def error(message: => String): ${ioname}[Unit] = FF.liftF[${opname}, Unit](LogError(() => message))
    |  def warn(message: => String): ${ioname}[Unit] = FF.liftF[${opname}, Unit](LogWarn(() => message))
    |  def info(message: => String): ${ioname}[Unit] = FF.liftF[${opname}, Unit](LogInfo(() => message))
    |  def debug(message: => String): ${ioname}[Unit] = FF.liftF[${opname}, Unit](LogDebug(() => message))
    |  def trace(message: => String): ${ioname}[Unit] = FF.liftF[${opname}, Unit](LogTrace(() => message))
    |
    |  // Smart constructors for $oname-specific operations.
    |  ${ctors[A].map(_.lifted(ioname)).mkString("\n  ")}
    |
    |  // ${ioname} is an Async
    |  implicit val Async${ioname}: Async[${ioname}] =
    |    new Async[${ioname}] {
    |      val asyncM = FF.catsFreeMonadForFree[${opname}]
    |      def bracketCase[A, B](acquire: ${ioname}[A])(use: A => ${ioname}[B])(release: (A, ExitCase[Throwable]) => ${ioname}[Unit]): ${ioname}[B] = module.bracketCase(acquire)(use)(release)
    |      def pure[A](x: A): ${ioname}[A] = asyncM.pure(x)
    |      def handleErrorWith[A](fa: ${ioname}[A])(f: Throwable => ${ioname}[A]): ${ioname}[A] = module.handleErrorWith(fa, f)
    |      def raiseError[A](e: Throwable): ${ioname}[A] = module.raiseError(e)
    |      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ${ioname}[A] = module.async(k)
    |      def asyncF[A](k: (Either[Throwable,A] => Unit) => ${ioname}[Unit]): ${ioname}[A] = module.asyncF(k)
    |      def flatMap[A, B](fa: ${ioname}[A])(f: A => ${ioname}[B]): ${ioname}[B] = asyncM.flatMap(fa)(f)
    |      def tailRecM[A, B](a: A)(f: A => ${ioname}[Either[A, B]]): ${ioname}[B] = asyncM.tailRecM(a)(f)
    |      def suspend[A](thunk: => ${ioname}[A]): ${ioname}[A] = asyncM.flatten(module.delay(thunk))
    |    }
    |
    |  // $ioname is a ContextShift
    |  implicit val ContextShift${ioname}: ContextShift[${ioname}] =
    |    new ContextShift[${ioname}] {
    |      def shift: ${ioname}[Unit] = module.shift
    |      def evalOn[A](ec: ExecutionContext)(fa: ${ioname}[A]) = module.evalOn(ec)(fa)
    |    }
    |
    |  // $ioname is a MessageLogger
    |  implicit val MessageLogger${ioname}: MessageLogger[${ioname}] =
    |    new MessageLogger[${ioname}] {
    |      def error(message: => String): ${ioname}[Unit] =  module.error(message)
    |      def warn(message: => String): ${ioname}[Unit] = module.warn(message)
    |      def info(message: => String): ${ioname}[Unit] = module.info(message)
    |      def debug(message: => String): ${ioname}[Unit] =  module.debug(message)
    |      def trace(message: => String): ${ioname}[Unit] =  module.trace(message)
    |    }
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
     |package $pkg
     |
     |import cats.free.Free
     |import com.github.ghik.silencer.silent
     |
     |${managed.map(ioImport).mkString("\n")}
     |
     |// A pair (J, Free[F, A]) with constructors that tie down J and F.
     |sealed trait Embedded[A]
     |
     |@silent("deprecated")
     |object Embedded {
     |  ${managed.map(ClassTag(_)).map(embed(_)).mkString("\n  ") }
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
     val mname  = oname.toLowerCase
     s"""
       |  trait ${oname}Interpreter extends ${oname}Op.Visitor[Kleisli[M, $sname, *]] {
       |
       |    // common operations delegate to outer interpreter
       |    override def raw[A](message: => String, f: $sname => A): Kleisli[M, $sname, A] = outer.raw(message, f)
       |    override def embed[A](e: Embedded[A]): Kleisli[M, $sname, A] = outer.embed(e)
       |    override def delay[A](a: () => A): Kleisli[M, $sname, A] = outer.delay(a)
       |    override def raiseError[A](err: Throwable): Kleisli[M, $sname, A] = outer.raiseError(err)
       |    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, $sname, A] = outer.async(k)
       |
       |    // for asyncF we must call ourself recursively
       |    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ${ioname}[Unit]): Kleisli[M, $sname, A] =
       |      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))
       |
       |    // for handleErrorWith we must call ourself recursively
       |    override def handleErrorWith[A](fa: ${ioname}[A], f: Throwable => ${ioname}[A]): Kleisli[M, $sname, A] =
       |      Kleisli { j =>
       |        val faʹ = fa.foldMap(this).run(j)
       |        val fʹ  = f.andThen(_.foldMap(this).run(j))
       |        asyncM.handleErrorWith(faʹ)(fʹ)
       |      }
       |
       |    def bracketCase[A, B](acquire: ${ioname}[A])(use: A => ${ioname}[B])(release: (A, ExitCase[Throwable]) => ${ioname}[Unit]): Kleisli[M, $sname, B] =
       |      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))
       |
       |    val shift: Kleisli[M, $sname, Unit] =
       |      Kleisli(_ => contextShiftM.shift)
       |
       |    def evalOn[A](ec: ExecutionContext)(fa: $ioname[A]): Kleisli[M, $sname, A] =
       |      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))
       |
       |    // Logger
       |    def error(message: => String): Kleisli[M, $sname, Unit] = outer.error(message)
       |    def warn(message: => String): Kleisli[M, $sname, Unit] = outer.warn(message)
       |    def info(message: => String): Kleisli[M, $sname, Unit] = outer.info(message)
       |    def debug(message: => String): Kleisli[M, $sname, Unit] = outer.debug(message)
       |    def trace(message: => String): Kleisli[M, $sname, Unit] = outer.trace(message)

       |    // domain-specific operations are implemented in terms of `primitive`
       |${ctors[A].map(_.kleisliImpl).mkString("\n")}
       |
       |  }
       |""".trim.stripMargin
    }

   def interpreterDef(c: Class[_]): String = {
     val oname = c.getSimpleName // original name, without name mapping
     val sname = toScalaType(c)
     val opname = s"${oname}Op"
     val ioname = s"${oname}IO"
     val mname  = oname.toLowerCase
     s"lazy val ${oname}Interpreter: ${opname} ~> Kleisli[M, $sname, *] = new ${oname}Interpreter { }"
   }


   // template for a kleisli interpreter
   def kleisliInterpreter: String =
     s"""
      |package $pkg
      |
      |// Library imports
      |import cats.~>
      |import cats.data.Kleisli
      |import cats.effect.{ Async, Blocker, ContextShift, ExitCase }
      |import cats.implicits._
      |import scala.concurrent.ExecutionContext
      |import scala.util.control.NonFatal
      |import com.github.ghik.silencer.silent
      |import io.chrisdavenport.log4cats.MessageLogger
      |
      |// Types referenced in the JDBC API
      |${managed.map(ClassTag(_)).flatMap(imports(_)).distinct.sorted.mkString("\n") }
      |
      |// Algebras and free monads thereof referenced by our interpreter.
      |${managed.map(_.getSimpleName).map(c => s"import ${pkg}.${c.toLowerCase}.{ ${c}IO, ${c}Op }").mkString("\n")}
      |
      |object KleisliInterpreter {
      |
      |  def apply[M[_]](b: Blocker)(
      |    implicit am: Async[M],
      |             cs: ContextShift[M],
      |             lo: MessageLogger[M],
      |  ): KleisliInterpreter[M] =
      |    new KleisliInterpreter[M] {
      |      val asyncM = am
      |      val loggerM = lo
      |      val contextShiftM = cs
      |      val blocker = b
      |    }
      |
      |}
      |
      |// Family of interpreters into Kleisli arrows for some monad M.
      |@silent("deprecated")
      |trait KleisliInterpreter[M[_]] { outer =>
      |
      |  implicit val asyncM: Async[M]
      |  implicit val loggerM: MessageLogger[M]
      |
      |  // We need these things in order to provide ContextShift[ConnectionIO] and so on, and also
      |  // to support shifting blocking operations to another pool.
      |  val contextShiftM: ContextShift[M]
      |  val blocker: Blocker
      |
      |  // The ${managed.length} interpreters, with definitions below. These can be overridden to customize behavior.
      |  ${managed.map(interpreterDef).mkString("\n  ")}
      |
      |  // Some methods are common to all interpreters and can be overridden to change behavior globally.
      |  def primitive[J, A](f: J => A, operation: => String, args: Any*): Kleisli[M, J, A] = Kleisli { a =>
      |    // primitive JDBC methods throw exceptions and so do we when reading values
      |    // so catch any non-fatal exceptions and lift them into the effect
      |    blocker.blockOn[M, A] {
      |      import scala.Predef._
      |
      |      def logMessage: String =
      |        s"$${a.getClass.getSimpleName}@$${Integer.toHexString(System.identityHashCode(a))} $${operation}$${if (args.isEmpty) "" else args.mkString("(", ",", ")")}"
      |
      |      (asyncM.delay(f(a)) <* loggerM.trace(logMessage)).onError {
      |        case NonFatal(e) => loggerM.error(doobie.free.ErrorFormatter.format(a, operation, args, e))
      |      }
      |
      |    } (contextShiftM)
      |  }
      |  def delay[J, A](a: () => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.delay(a()))
      |  def raw[J, A](message: => String, f: J => A): Kleisli[M, J, A] = primitive(f, message)
      |  def raiseError[J, A](e: Throwable): Kleisli[M, J, A] = Kleisli(_ => asyncM.raiseError(e))
      |  def async[J, A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, J, A] = Kleisli(_ => asyncM.async(k))
      |  def embed[J, A](e: Embedded[A]): Kleisli[M, J, A] =
      |    e match {
      |      ${managed.map(_.getSimpleName).map(n => s"case Embedded.${n}(j, fa) => Kleisli(_ => fa.foldMap(${n}Interpreter).run(j))").mkString("\n      ")}
      |    }
      |
      |  // Logger
      |  def error[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.error(message))
      |  def warn[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.warn(message))
      |  def info[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.info(message))
      |  def debug[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.debug(message))
      |  def trace[J](message: => String): Kleisli[M, J, Unit] = Kleisli(_ => loggerM.trace(message))
      |
      |  // Interpreters
      |${managed.map(ClassTag(_)).map(interp(_)).mkString("\n")}
      |
      |}
      |""".trim.stripMargin

  def gen(base: File): Seq[java.io.File] = {
    import java.io._
    log.info("Generating free algebras into " + base)
    val fs = managed.map { c =>
      base.mkdirs
      val mod  = module(ClassTag(c))
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

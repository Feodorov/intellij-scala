//scala/test/files/run/macro-expand-implicit-macro-is-implicit/Macros_Test_2.scala
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object MacroIsImplicit {
  def foo(c: Context)(x: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    c.Expr[Int](q"1")
  }
}


object Test extends App {
  import scala.language.implicitConversions
  implicit def foo(x: String): Any = macro MacroIsImplicit.foo
  val z: Any = "2"
  /*start*/z/*end*/
}
//Int
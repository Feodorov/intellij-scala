//scala/test/files/run/macro-expand-implicit-macro-has-implicit
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object ImplicitParameter {
  def foo(c: Context)(x: c.Expr[Int]): c.Expr[Any] = {
    import c.universe._
    c.Expr[Int](q"$x + 2")
  }
}

object Test extends App {
  implicit val x = 42
  def foo(implicit x: Int): Any = macro ImplicitParameter.foo
  /*start*/foo/*end*/
}
//Int
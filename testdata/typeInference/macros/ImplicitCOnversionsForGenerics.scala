package macroexample
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

class A
class B extends A

object MacroAsImplicitParameter {
  def foo(c: Context)(i: c.Expr[Int]): c.Expr[List[A]] = {
    import c.universe._
    c.Expr[List[B]](q"List(new B)")
  }
}

object TestImplicitListB extends App {
  implicit def foo(i: Int): List[A] = macro MacroAsImplicitParameter.foo
  def bar[T](x: List[T]): List[T] = x
  /*start*/bar(1)/*end*/
}
//List[B]
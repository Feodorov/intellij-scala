package macroexample
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

class A
class B extends A

object MacroAsImplicitParameter {
  def foo(c: Context): c.Expr[List[A]] = {
    import c.universe._
    c.Expr[List[B]](q"List(new B)")
  }
}

object TestList extends App {
  implicit def foo: List[A] = macro MacroAsImplicitParameter.foo
  def bar[T](implicit x: List[T]): List[T] = x
  /*start*/bar/*end*/
}
//List[B]
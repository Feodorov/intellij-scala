//scala/test/files/run/macro-expand-implicit-macro-is-val/Impls_1.scala
package macroexample
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

class A
class B extends A

object MacroAsImplicitParameter {
  def foo(c: Context): c.Expr[A] = {
    import c.universe._
    c.Expr[B](q"new B")
  }
}

object Test extends App {
  implicit def foo: A = macro MacroAsImplicitParameter.foo
  def bar(implicit x: A): x.type = x
  /*start*/bar/*end*/
}
//B
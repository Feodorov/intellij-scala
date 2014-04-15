//scala/test/files/run/macro-expand-implicit-macro-is-implicit/Macros_Test_2.scala
package macroexample
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

class A
class B extends A

object MacroIsImplicit {
  def foo(c: Context)(x: c.Expr[String]): c.Expr[A] = {
    import c.universe._
    c.Expr[B](q"new B")
  }
}

object Test extends App {
  import scala.language.implicitConversions
  implicit def foo(x: String): A = macro MacroIsImplicit.foo
  val z: A = /*start*/"2"/*end*/
}
//B
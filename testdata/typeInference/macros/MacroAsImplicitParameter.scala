//scala/test/files/run/macro-expand-implicit-macro-is-val/Impls_1.scala
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object MacroAsImplicitParameter {
  def foo(c: Context): c.Expr[AnyVal] = {
    import c.universe._
    val body = Literal(Constant(2))
    c.Expr[Int](body)
  }
}

object Test extends App {
  implicit def foo: AnyVal = macro MacroAsImplicitParameter.foo
  def bar(implicit x: AnyVal): x.type = x
  /*start*/bar/*end*/
}
//Int
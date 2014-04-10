import _root_.example.MyIntMacroInClass
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object MyIntMacro {
  def impl(c: Context)(x: c.Tree): c.Expr[Any] = {
    import c.universe._
    c.Expr[Int](q"x + 1")
  }
}

class MyIntMacroInClass {
  def macroFunction(x: Int): Any = macro MyIntMacro.impl
}

object Test {
  /*start*/(new MyIntMacroInClass).macroFunction(3)/*end*/
}
//Int
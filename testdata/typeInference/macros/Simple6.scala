import _root_.example.MyIntMacroInClass
import _root_.example.MyParamlessMacroInClass
import language.experimental.macros
import scala.reflect.macros.whitebox.Context

object MyParamlessIntMacro {
  def method: Any = macro impl
  def impl(c: Context): c.Expr[Any] = {
    import c.universe._
    c.Expr[Int](q"1 + 1")
  }
}

class MyParamlessMacroInClass {
  def macroFunction: Any = macro MyParamlessIntMacro.impl
}

object Test {
  /*start*/new MyParamlessMacroInClass macroFunction/*end*/
}
//Int
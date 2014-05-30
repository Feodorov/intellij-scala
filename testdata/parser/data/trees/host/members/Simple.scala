def f(a: Int)(implicit b: Int) = a + b
//List(Aux.Param.Named(List(), Term.Name(a), Some(Type.Name(Int)), None), Aux.Param.Named(List(), Term.Name(b), Some(Type.Name(Int)), None))
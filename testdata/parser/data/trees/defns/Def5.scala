def f(a: Int)(implicit b: Int) = a + b
//Defn.Def(List(), Term.Name(f), List(), List(List(Aux.Param.Named(List(), Term.Name(a), Some(Type.Name(Int)), None))), List(Aux.Param.Named(List(), Term.Name(b), Some(Type.Name(Int)), None)), None, Term.ApplyInfix(Term.Name(a), Term.Name(+), List(), List(Term.Name(b))))
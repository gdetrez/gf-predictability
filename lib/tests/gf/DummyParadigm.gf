resource DummyParadigm = 
  open (Predef=Predef), Prelude in {

oper

  mkDummy = overload {
    mkDummy : Str -> { a:Str ; b:Str ; c:Str } = \a -> 
      { a = a
      ; b = a
      ; c = a
      } ;
    mkDummy : Str -> Str -> { a:Str ; b:Str ; c:Str } = \a,b -> 
      { a = a
      ; b = b
      ; c = b
      } ;
    mkDummy : Str -> Str -> Str -> { a:Str ; b:Str ; c:Str } = \a,b,c -> 
      { a = a
      ; b = b
      ; c = c
      } ;
  } ;
} ;

resource ParadigmEng = open LexiconEng in {
  oper mkN = overload {
    mkN : Str -> N
      = \s -> lin N { s = table Number [s; s + "s"] } ;
    mkN : Str -> Str -> N
      = \s1,s2 -> lin N { s = table Number [s1; s2] }
  } ;

  oper mkV = overload {
    mkV : Str -> V
      = \s -> lin V { s = table Tense [s; s + "ed"; s + "ed"] } ;
    mkV : Str -> Str -> V
      = \s1,s2 -> lin V { s = table Tense [s1; s2; s2] } ;
    mkV : Str -> Str -> Str -> V
      = \s1,s2,s3 -> lin V { s = table Tense [s1; s2; s3] }
  } ;
}

--# -path=.:../abstract:../common:../../prelude

resource ShortParadigmsFin = open 
  ParadigmsFin,
  MorphoFin,
  CatFin
  in {

  flags optimize=noexpand ;

oper
  mkNForms = overload {
    mkNForms : (talo : Str) -> NForms = nForms1 ;
    mkNForms : (talo,taloja : Str) -> NForms = nForms2 ;
    mkNForms : (talo,talon,taloja : Str) -> NForms = nForms3 ;
    mkNForms : (talo,talon,taloja,taloa : Str) -> NForms = nForms4 ;
    mkNForms : (talo,talon,taloa,talona,taloon,talojen,taloja,taloina,taloissa,taloihin
        : Str) -> NForms = nForms10 ;
    } ;

  mkVForms = overload {
    mkVForms : (huutaa : Str) -> VForms = vForms1 ;
    mkVForms : (huutaa,huusi : Str) -> VForms = vForms2 ;
    mkVForms : (
      huutaa,huudan,huutaa,huutavat,huutakaa,huudetaan,
      huusin,huusi,huusisi,huutanut,huudettu,huutanee : Str) -> VForms = vForms12 ;
  } ;


} ;

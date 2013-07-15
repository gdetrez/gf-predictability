concrete LexiconEng of Lexicon = {

  param Number = Sg | Pl ;
  param Tense  = Present | Past | Participle ;

  lincat N = { s : Number => Str } ;
  lincat V = { s : Tense  => Str } ;

  lin cat_  = { s = table Number ["cat"; "cats"] } ;
  lin dog   = { s = table Number ["dog"; "dogs"] } ;
  lin mouse = { s = table Number ["mouse"; "mices"] } ;

  lin eat   = { s = table Tense [ "eat"; "ate"; "eaten"] } ;
  lin pray  = { s = table Tense [ "pray"; "prayed"; "prayed" ] } ;
  lin love  = { s = table Tense [ "love"; "loved"; "loved" ] } ;

}

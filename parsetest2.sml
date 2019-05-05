structure Parse  =
struct 
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
  fun parse filename =
      let 
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
          fun parseerror(s, i:int, _) = TextIO.output(TextIO.stdErr,
			"Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

	  val (absyn, _) = TigerP.parse(0, lexer, parseerror,())

       in TextIO.closeIn file;
	   absyn
      end 

end

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val eof = fn () => Tokens.EOF(0,0)

%%
%header (functor TigerLexFun (structure Tokens: Tiger_TOKENS));

newline = "\\n" | "\\r" | "\\n\\r" | "\\r\\n";
ws = [\ \t];
%%
[\ \r\t\n]       => (lex());

"#include"{ws}+"<stdio.h>" => (Tokens.HEADER (yypos, yypos+18));

"("              => (Tokens.LPAREN  (yypos, yypos+1));
")"	         => (Tokens.RPAREN  (yypos, yypos+1));
"{"	         => (Tokens.LCURLY  (yypos, yypos+1));
"}"	         => (Tokens.RCURLY  (yypos, yypos+1));
"["              => (Tokens.LSQUARE (yypos, yypos+1));
"]"		 => (Tokens.RSQUARE (yypos, yypos+1));

";"              => (Tokens.SEMICOLON (yypos, yypos+1));
","              => (Tokens.COMMA      (yypos, yypos+1));
":"              => (Tokens.COLON      (yypos, yypos+1));

"+"              => (Tokens.PLUS  (yypos, yypos+1));
"-"              => (Tokens.MINUS (yypos, yypos+1));
"*"              => (Tokens.MUL   (yypos, yypos+1));
"/"              => (Tokens.DIV   (yypos, yypos+1));
"%"              => (Tokens.MOD   (yypos, yypos+1));

"="              => (Tokens.EQUAL   (yypos, yypos+1));
"!"              => (Tokens.NOT     (yypos, yypos+1));
"~"              => (Tokens.TILDA   (yypos, yypos+1));
"?"              => (Tokens.QUES    (yypos, yypos+1));
"."              => (Tokens.DOT     (yypos, yypos+1));

"print"          => (Tokens.PRINT    (yypos, yypos+5));
"record"         => (Tokens.RECORD   (yypos, yypos+6));
"static"	 => (Tokens.STATIC   (yypos, yypos+6));
"return"         => (Tokens.RETURN   (yypos, yypos+6));
"void"           => (Tokens.VOID     (yypos, yypos+4));
"break"          => (Tokens.BREAK    (yypos, yypos+6));
"continue"       => (Tokens.CONTINUE (yypos, yypos+6));

"int"            => (Tokens.INT   (yypos, yypos+3));
"bool"		 => (Tokens.BOOL  (yypos, yypos+4));
"char"           => (Tokens.CHAR  (yypos, yypos+4));
"true"           => (Tokens.TRUE  (yypos, yypos+4));
"false"          => (Tokens.FALSE (yypos, yypos+5));

"if"             => (Tokens.IF    (yypos, yypos+2));
"else"           => (Tokens.ELSE  (yypos, yypos+4));
"while"          => (Tokens.WHILE (yypos, yypos+5));
"for"            => (Tokens.FOR   (yypos, yypos+3));

">"              => (Tokens.GREATER      (yypos, yypos+1));
"<"              => (Tokens.LESS         (yypos, yypos+1));
"<="             => (Tokens.LESSEQUAL    (yypos, yypos+2));
">="             => (Tokens.GREATEREQUAL (yypos, yypos+2));
"!="             => (Tokens.NOTEQUAL     (yypos, yypos+2));
"=="             => (Tokens.EQUALEQUAL   (yypos, yypos+2));

"+="             => (Tokens.PLUSEQUAL (yypos, yypos+2));
"-="             => (Tokens.MINUSEQUAL (yypos, yypos+2));
"*="             => (Tokens.MULEQUAL (yypos, yypos+2));
"/="             => (Tokens.DIVEQUAL (yypos, yypos+2));

"++"             => (Tokens.INCREMENT (yypos, yypos+2));
"--"             => (Tokens.DECREMENT (yypos, yypos+2));

"||"             => (Tokens.OR  (yypos, yypos+2));
"&&"             => (Tokens.AND (yypos, yypos+2));
"//".*           => (lex());

[0-9]+           => (Tokens.NUMCONST (yytext, yypos, yypos + (String.size yytext)));

[a-zA-Z_][a-zA-Z0-9_]*  => (Tokens.ID (yytext, yypos, yypos + (String.size yytext)));

"\""({newline} | [^\\"])*"\""          => (Tokens.CHARCONST (yytext, yypos, yypos+(String.size yytext)));

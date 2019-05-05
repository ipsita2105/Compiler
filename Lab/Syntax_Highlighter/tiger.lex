type lexresult = Tokens.token
fun eof () = Tokens.EOF (0,0)

%%
digit=[0-9];
whitespace=[\ \r\t\n];
alpha=[A-Za-z];
%%

[\ \r\t\n]   => (Tokens.WHITESPACE (yytext));
"let" => (Tokens.LET ( ));
"in" => (Tokens.IN ( ));
"end" => (Tokens.END ( ));
"type" => (Tokens.TYPE ());
"array" => (Tokens.ARRAY ());
"int" => (Tokens.INT());
"=" => (Tokens.EQUAL());
":" => (Tokens.COLON());
"var" => (Tokens.VAR());
{alpha}+ => (Tokens.ALPHA (yytext));
{digit}+ => (Tokens.NUM (yytext));
\/\*([^*]|[\n])*\*\/ => (Tokens.COMMENT (yytext));

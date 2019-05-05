structure Tokens = 
struct
type token = string
fun NEWLINE () = "\n"
fun WHITESPACE (x) = x
fun LET () = "\027[1;31mlet\027[0m"
fun IN () = "\027[1;31min\027[0m"
fun END () = "\027[1;31mend\027[0m"
fun ALPHA (x) = x
fun TYPE () = "\027[1;33mtype\027[0m"
fun ARRAY () = "\027[1;34marray\027[0m"
fun INT () = "\027[1;34mint\027[0m"
fun VAR () = "\027[1;34mvar\027[0m"
fun EQUAL () = "="
fun COLON () = ":"
fun NUM   (x) = x
fun COMMENT (x) = "\027[1;35m"^x^"\027[0m" 
fun EOF   (i, j) = "EOF"
end

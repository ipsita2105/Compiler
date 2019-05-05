structure Pretty : sig val pp: Absyn.exp -> unit end = 

struct

        structure A = Absyn

        val fd = TextIO.openOut "output"

        fun opsymbol A.PlusOp = " + "
          | opsymbol A.EqOp   = " = " 

        fun pp (A.LetExp{decs, body, pos}) = ( TextIO.output(fd, "let\n\t"); 
                                               pp decs; 
                                               TextIO.output(fd,"\nin\n\t"); 
                                               pp body; 
                                               TextIO.output(fd,"\nend\n")) 

         (*| pp  (A.ExpList (x::(A.ExpList xs))) = (TextIO.output(fd, "\n\t"); pp x; pp xs)
         | pp  (A.ExpList ([]))    = ()*)

         | pp  (A.OpExp{left, oper, right, pos}) = (pp left; 
                                                    TextIO.output(fd, opsymbol oper); 
                                                    pp right)

         | pp  (A.IfExp{cond, body1, body2, pos}) = (TextIO.output(fd,"if "); 
                                                     pp cond; 
                                                     TextIO.output(fd," then\n\t"); 
                                                     pp body1;
                                                     TextIO.output(fd,"\nelse\n\t"); 
                                                     pp body2;
                                                     TextIO.output(fd, "\n"))

         | pp  (A.StringExp(s, p)) = TextIO.output(fd, s)

         | pp  (A.IntExp(i)) = TextIO.output(fd, Int.toString(i))
end

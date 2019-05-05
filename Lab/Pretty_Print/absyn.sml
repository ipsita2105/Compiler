structure Absyn = 
struct

  type pos     = int
  datatype exp = 
                  IntExp of int
                | StringExp of string*pos
                | ExpList of exp list
                | IfExp of {cond:exp, body1:exp, body2:exp, pos:pos}
                | LetExp of {decs:exp, body:exp, pos:pos}
                | OpExp of {left: exp, oper:oper, right: exp, pos:pos}

  and oper =  EqOp
            | PlusOp

end

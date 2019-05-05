structure CodeGen =
struct

structure Ast = Absyn

open printColor;

val indent = ref 0
val n = ref 0
fun ptabs  () = if !n = 0 then ( print("")) else (n := !n - 1 ; print ("\t") ; ptabs ());
fun prtabs () = (n := !indent ; ptabs () )

val fd = TextIO.openOut "output.js"
fun file_print (x) = (TextIO.output (fd, x))


(******************************** main program *****************************************)

fun code_generator (Ast.declL(x)) = (print_decl_list x; file_print ("\nmain();\n"))

and   print_decl_list   ([])   = ()
    | print_decl_list   ([x])  = (print_decl x)
    | print_decl_list   (x::y) = (print_decl x ; print_decl_list y)

and   print_decl (Ast.variableDeclaration (x)) = (print_var_decl x)
    | print_decl (Ast.functionDeclaration (x)) = (print_fun_decl x)

(* ===================================== variable declaration ================================= *)

and print_var_decl (Ast.vDecl(x,y)) = (      prtabs () ; 
                                             print_type_specifier x ; 
                                             print_var_decl_list (x, y) ; 
                                             file_print ";\n")


and   print_var_decl_list (t:Ast.typeSpecifier, [])   = () 
    | print_var_decl_list (t:Ast.typeSpecifier, [x])  = (print_var_decl_init (t,x))
    | print_var_decl_list (t:Ast.typeSpecifier, x::y) = (print_var_decl_init
    (t,x) ; file_print " , " ; print_var_decl_list (t,y))


and   print_var_decl_init   (t:Ast.typeSpecifier, Ast.declOnlyID (x))   = (      print_var_decl_id (t,x))
    | print_var_decl_init   (t:Ast.typeSpecifier, Ast.declAssign (x,y)) = (      print_var_decl_id (t, x) ; 
                                                                                 file_print " = " ; 
                                                                                 print_simple_exp (y)
                                                                                 )


and   print_var_decl_id (t:Ast.typeSpecifier, Ast.vID (x)) = (file_print x)

    | print_var_decl_id (t:Ast.typeSpecifier, Ast.arrayDecl (x,y)) = (  
                                           
                                            file_print x;
                                            file_print " = []")

and print_type_specifier (Ast.integer)   = (file_print "var ")
  | print_type_specifier (Ast.boolean)   = (file_print "var ")
  | print_type_specifier (Ast.character) = (file_print "var ")
 
(* ===================================== function declaration ============================== *)

and print_fun_decl (Ast.funReturn (x,y,z,w)) = (
                                       
                                                            (*print_type_specifier x;*)
                                                            file_print "function " ; 
                                                            file_print y ;
                                                            file_print " ";
                                                            file_print "( ";
                                                            print_param_list z;
                                                            file_print " )";
                                                            print_cmpd_stmt w
                                                         )

    | print_fun_decl (Ast.funVoid (y,z,w)) = (     
                                                      
                                                            (*file_print "void";*)
                                                            file_print "function " ; 
                                                            file_print y ;
                                                            file_print " ";
                                                            file_print "( ";
                                                            print_param_list z;
                                                            file_print " )";
                                                            print_cmpd_stmt w

                                                         )


and  print_param_list ([])    = ()
   | print_param_list ([x])   = (print_param_type_list x)
   | print_param_list (x::y)  = (print_param_type_list x ; file_print ", " ; print_param_list y)

and print_param_type_list (Ast.parameter(x,y)) = ( print_param_id y)


and   print_param_id   (Ast.justID(x))     = (file_print x ; file_print " ")
    | print_param_id   (Ast.arrayID (x))   = (file_print x ; file_print " " )

(* ============================= for the statements ============================ *)
and  print_stmt (Ast.eStatement(x))   = (prtabs () ; print_exp_stmt x)
   | print_stmt (Ast.cStatement(x))   = (prtabs () ; print_cmpd_stmt x)
   | print_stmt (Ast.sStatement(x))   = (prtabs () ; print_select_stmt x)
   | print_stmt (Ast.iStatement(x))   = (prtabs () ; print_iter_stmt x)
   | print_stmt (Ast.rStatement(x))   = (prtabs () ; print_ret_stmt x)
   | print_stmt (Ast.bStatement(x))   = (prtabs () ; print_break_stmt x)
   | print_stmt (Ast.conStatement(x)) = (prtabs () ; print_cont_stmt x)
   | print_stmt (Ast.printStatement(x)) = (prtabs (); printStmt x)
and print_cmpd_stmt (Ast.braceStmt (x,y)) = (

                                                        file_print "{\n";
                                                        indent := !indent + 1;
                                                        print_local_decl x;
                                                        print_stmtList y;
                                                        prtabs();
                                                        file_print "}";
                                                        file_print "\n";

                                                                
                                                        (*(print("******HashTable**********\n"));
                                                        (Symbol.print_hash_table(!Htable));
                                                        (print("******Stack*********\n"));
                                                        (Stack.print_symbol_stack(!GStack));
                                                        (print("\n"));*)

                                                        indent := !indent - 1
                                                        ) 

and print_local_decl (Ast.ldecls(x,y)) = (
                                                print_local_decl x;
                                                print_var_decl y
                                                )

    | print_local_decl (Ast.emptyldecls ) = ()

and   print_stmtList ([])     = () 
    | print_stmtList (x::y)   = (print_stmt x ; print_stmtList y)

and print_exp_stmt (Ast.sExpression (x)) = (print_exp (x) ; file_print ";\n") 
    | print_exp_stmt (Ast.semicolon) = (file_print ";\n")


and print_select_stmt (Ast.IF (x,y)) = (
                                            file_print "if ";
                                            file_print "(";
                                            print_simple_exp (x);
                                            file_print " )";
                                            print_stmt y
                                        )

    | print_select_stmt (Ast.IFELSE (x,y,z)) = (
                                                    file_print "if ";
                                                    file_print "( ";
                                                    print_simple_exp (x);
                                                    file_print " )";
                                                    print_stmt y;
                                                    prtabs();
                                                    file_print "else";
                                                    print_stmt z
                                                )

and print_iter_stmt (Ast.WHILE (x,y)) = (
                                                file_print "while ";
                                                file_print "( ";
                                                print_simple_exp (x);
                                                file_print " )";
                                                print_stmt y
                                           )

and   print_ret_stmt (Ast.voidReturn)      = ( file_print " return " ; file_print ";\n")
    | print_ret_stmt (Ast.returnValue (x)) = ( file_print " return " ; print_exp (x) ; file_print ";\n" )

and print_break_stmt (Ast.BREAK)   = (file_print "break ;\n") 
and print_cont_stmt (Ast.CONTINUE) = (file_print "continue ;\n")

and printStmt (Ast.printing (x))  = (file_print "console.log("; print_exp_list x; file_print ");\n")

and print_exp_list ([]) = ()
 |  print_exp_list ([x]) = (print_simple_exp (x))
 |  print_exp_list (x::xs) = (print_simple_exp x; file_print ", "; print_exp_list xs)

(* ================================== expression ==================================== *)
and print_exp   (Ast.assign (x, y))      = (                 print_mutable (x) ; 
                                                             file_print " = " ; 
                                                             print_exp (y)
                                                             )


    | print_exp (Ast.assignPlus (x, y))  = (                 print_mutable (x) ;
                                                             file_print " += " ;
                                                             print_exp (y)
                                                             )


    | print_exp (Ast.assignMinus (x, y)) = (                print_mutable (x) ; 
                                                            file_print " -= " ; 
                                                            print_exp (y)
                                                            )


    | print_exp (Ast.assignMul (x, y))   = (                print_mutable (x) ; 
                                                            file_print " *= " ; 
                                                            print_exp (y)
                                                            )
    

    | print_exp (Ast.assignDiv (x, y))   = (                print_mutable (x) ; 
                                                            file_print " /= " ; 
                                                            print_exp (y)
                                                            )
    

    | print_exp (Ast.increment (x))      = (               print_mutable (x);
                                                           file_print "++ "
                                                           )


    | print_exp (Ast.decrement (x))      = (               print_mutable (x); 
                                                           file_print "--"
                                                           ) 


    | print_exp (Ast.simpExpression (x)) = (               print_simple_exp (x)
                                                           )


and print_simple_exp   (Ast.or (x,y))    = (               print_simple_exp (x);
                                                           file_print " || " ; 
                                                           print_and_exp (y)
                                                           )

    | print_simple_exp (Ast.no_or(x))   = (                print_and_exp (x))


and print_and_exp   (Ast.andExp (x, y)) = (                print_and_exp (x) ; 
                                                           file_print " && " ; 
                                                           print_unary_rel_exp (y)) 

    | print_and_exp (Ast.uExpr (x))     = (                print_unary_rel_exp (x))


and print_unary_rel_exp   (Ast.not (x)) = (                file_print " !" ; 
                                                           print_unary_rel_exp (x)
                                                           )

    | print_unary_rel_exp (Ast.rExpr(x)) = (               print_rel_exp (x))


and print_rel_exp   (Ast.relExp (x,y,z)) = (               print_sum_exp (x) ;
                                                           print_relop y; 
                                                           print_sum_exp (z)
                                                           )

    | print_rel_exp (Ast.noRel (x))     = (                print_sum_exp (x))


and   print_relop (Ast.LTE) = file_print " <= "
    | print_relop (Ast.LT)  = file_print " < "
    | print_relop (Ast.GTE) = file_print " >= "
    | print_relop (Ast.GT)  = file_print " > "
    | print_relop (Ast.EQ)  = file_print " == "
    | print_relop (Ast.NEQ) = file_print " != "


and print_sum_exp (Ast.sumExp (x,y,z)) = (      print_sum_exp (x) ;
                                                print_sumop y; 
                                                print_term (z)
                                                )

    | print_sum_exp (Ast.onlyTerm (x)) = (      print_term (x))


and print_sumop   (Ast.PLUS)  = file_print " + "
    | print_sumop (Ast.MINUS) = file_print " - "


and print_term (Ast.mulExp (x,y,z))  = (

                                        print_term (x) ; 
                                        print_mulop y;  
                                        print_unary_exp (z))

    | print_term (Ast.nomul (x)) = (    print_unary_exp (x))


and   print_mulop (Ast.MUL) = file_print " * "
    | print_mulop (Ast.DIV) = file_print " / "
    | print_mulop (Ast.MOD) = file_print " % "

and print_unary_exp (Ast.uExp (x,y)) = (      print_unary_op x; 
                                              print_unary_exp (y)
                                              )

    | print_unary_exp (Ast.fac (x)) = (       print_fac (x))


and  print_unary_op (Ast.QUES)  = file_print " ? "


and   print_fac (Ast.mut (x))   = (print_mutable (x))
    | print_fac (Ast.immut (x)) = (print_immutable (x))



and print_mutable ( Ast.mID (x)) = ( file_print x )

    | print_mutable ( Ast.mArray (x,y)) = (

                                         print_mutable (x) ;
                                         file_print "["; 
                                         print_exp (y) ;
                                         file_print "]")


and print_immutable   (Ast.parans (x)) = (file_print "( " ; print_exp (x); file_print " )")
    | print_immutable (Ast.imc (x))    = (print_call (x)) 
    | print_immutable (Ast.const (x))  = (print_const (x))


and print_call (Ast.callArgs (x , y)) = (  
                                         file_print x ; 
                                         file_print " (" ; 
                                         print_arg_list (y) ; 
                                         file_print " )")


and print_arg_list ([])        = ()
    | print_arg_list ([x])     = (print_exp (x) )
    | print_arg_list (x::y)    = (print_exp (x) ; file_print ", " ; print_arg_list y)


and print_const   (Ast.number (x))    = (file_print x)
    | print_const (Ast.charConst (x)) = (file_print x)
    | print_const (Ast.true_val )     = (file_print "true")
    | print_const (Ast.false_val )    = (file_print "false")
end

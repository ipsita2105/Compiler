structure PrettyPrint =
struct

structure Ast = Absyn

open printColor;

val indent = ref 0
val n = ref 0
fun ptabs () = if !n = 0 then ( print("")) else (n := !n - 1 ; print ("\t") ; ptabs ());
fun prtabs () = (n := !indent ; ptabs () )


(******************************** main program *****************************************)

fun pprint (Ast.declL(x)) = (print_yellow ("#include <stdio.h>\n"); print_decl_list x; Semantic.check_for_main ())

and   print_decl_list   ([])   = ()
    | print_decl_list   ([x])  = (print_decl x)
    | print_decl_list   (x::y) = (print_decl x ; print_decl_list y)

and   print_decl (Ast.variableDeclaration (x)) = (print_var_decl x)
    | print_decl (Ast.functionDeclaration (x)) = (print_fun_decl x)

(* ===================================== variable declaration ================================= *)

and print_var_decl (Ast.vDecl(x,y)) = (      prtabs () ; 
                                             print_type_specifier x ; 
                                             print_var_decl_list (x, y) ; 
                                             print ";\n")


and   print_var_decl_list (t:Ast.typeSpecifier, [])   = () 
    | print_var_decl_list (t:Ast.typeSpecifier, [x])  = (print_var_decl_init (t,x))
    | print_var_decl_list (t:Ast.typeSpecifier, x::y) = (print_var_decl_init (t,x) ; print " , " ; print_var_decl_list (t,y))


and   print_var_decl_init   (t:Ast.typeSpecifier, Ast.declOnlyID (x))   = (print_var_decl_id (t,x))
    | print_var_decl_init   (t:Ast.typeSpecifier, Ast.declAssign (x,y)) = (      print_var_decl_id (t, x) ; 
                                                                                 print_default " = " ; 
                                                                                 print_simple_exp (y);
                                                                                TypeCheck.typecheck_val_init (t, y))


and   print_var_decl_id (t:Ast.typeSpecifier, Ast.vID (x)) = (  Scope.scopeInsert(Symbol.symbol(x), Semantic.get_type(t));
                                                                
                                                                (*(print("******HashTable**********\n"));
                                                                (Symbol.print_hash_table(!Htable));
                                                                (print("******Stack*********\n"));
                                                                (Stack.print_symbol_stack(!GStack));
                                                                (print("\n"));*)

                                                                print_default x)

    | print_var_decl_id (t:Ast.typeSpecifier, Ast.arrayDecl (x,y)) = (  
                                           
                                            Scope.scopeInsert(Symbol.symbol(x), Semantic.get_array_type (t));

                                            (*(print("*******HashTable**********\n"));
                                            (Symbol.print_hash_table(!Htable));
                                            (print("******Stack*********\n"));
                                            (Stack.print_symbol_stack(!GStack));
                                            (print("\n"));*)

                                            print_default x;
                                            print "[";
                                            print_default y;
                                            print "]"

                                            )

and print_type_specifier (Ast.integer)   = (print_cyan "int ")
  | print_type_specifier (Ast.boolean)   = (print_cyan "bool ")
  | print_type_specifier (Ast.character) = (print_cyan "char ")
 
(* ===================================== function declaration ============================== *)

and print_fun_decl (Ast.funReturn (x,y,z,w)) = (
                                       
                                       Scope.scopeInsert(Symbol.symbol(y), Semantic.get_fun_type (x, z));
                                       Scope.beginScope();                
                                       Scope.scopeInsertFunction(z);

                                       Semantic.found_main (y);

                                                            TypeCheck.update_last_return_type (x);
                                                            print_type_specifier x;
                                                            print "" ; print_magneta y ; print " ";
                                                            print "( ";
                                                            print_param_list z;
                                                            print " )";
                                                            print_cmpd_stmt w
                                                         )
    | print_fun_decl (Ast.funVoid (y,z,w)) = (     
                                                      
                                       Scope.scopeInsert(Symbol.symbol(y), Semantic.get_voidfun_type (z));
                                       Scope.beginScope();
                                       Scope.scopeInsertFunction(z);

                                       Semantic.found_main (y);
                                                             
                                                            TypeCheck.update_return_type_void ();
                                                            print_red "void ";
                                                            print "" ; print_magneta y ; print " ";
                                                            print "( ";
                                                            print_param_list z;
                                                            print " )";
                                                            print_cmpd_stmt w

                                                         )


and  print_param_list ([])    = ()
   | print_param_list ([x])   = (print_param_type_list x)
   | print_param_list (x::y)  = (print_param_type_list x ; print ", " ; print_param_list y)

and print_param_type_list (Ast.parameter(x,y)) = (print_type_specifier x ; print_param_id y)


and   print_param_id   (Ast.justID(x))   = (print_default x ; print " ")
    | print_param_id (Ast.arrayID (x))   = (print_default x ; print " [] " )

(* ============================= for the statements ============================ *)
and  print_stmt (Ast.eStatement(x))   = (prtabs () ; print_exp_stmt x)
   | print_stmt (Ast.cStatement(x))   = (prtabs () ; print_cmpd_stmt x)
   | print_stmt (Ast.sStatement(x))   = (prtabs () ; print_select_stmt x)
   | print_stmt (Ast.iStatement(x))   = (prtabs () ; print_iter_stmt x)
   | print_stmt (Ast.rStatement(x))   = (prtabs () ; print_ret_stmt x)
   | print_stmt (Ast.bStatement(x))   = (prtabs () ; print_break_stmt x)
   | print_stmt (Ast.conStatement(x)) = (prtabs () ; print_cont_stmt x)
   | print_stmt (Ast.printStatement(x)) = (prtabs (); print_pstmt x)

and print_cmpd_stmt (Ast.braceStmt (x,y)) = (

                                                        Scope.beginScope();
                                                        print_yellow "{\n";
                                                        indent := !indent + 1;
                                                        print_local_decl x;
                                                        print_stmtList y;
                                                        prtabs();
                                                        print_yellow "}";
                                                        print "\n";

                                                        Scope.endScope();
                                                                
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

and print_exp_stmt (Ast.sExpression (x)) = (print_exp (x) ; print ";\n") 
    | print_exp_stmt (Ast.semicolon) = (print ";\n")


and print_select_stmt (Ast.IF (x,y)) = (
                                            print_red "if ";
                                            print_yellow "(";
                                            print_simple_exp (x);
                                            TypeCheck.typecheck_if(x);
                                            print_yellow " )";
                                            print_stmt y
                                        )

    | print_select_stmt (Ast.IFELSE (x,y,z)) = (
                                                    print_red "if ";
                                                    print_yellow "( ";
                                                    print_simple_exp (x);
                                                    TypeCheck.typecheck_if (x);
                                                    print_yellow " )";
                                                    print_stmt y;
                                                    prtabs();
                                                    print_red "else";
                                                    print_stmt z
                                                )

and print_iter_stmt (Ast.WHILE (x,y)) = (
                                                print_red "while ";
                                                print_yellow "( ";
                                                print_simple_exp (x);
                                                print_yellow " )";
                                                TypeCheck.typecheck_while (x);
                                                print_stmt y
                                           )

and   print_ret_stmt (Ast.voidReturn)      = (print_red " return " ; print_yellow ";\n"; TypeCheck.check_void_return_type ())
    | print_ret_stmt (Ast.returnValue (x)) = (print_red " return "; print_exp (x) ; print_yellow ";\n"; TypeCheck.check_return_type (x) )

and print_break_stmt (Ast.BREAK)   = (print "break ;\n") 
and print_cont_stmt (Ast.CONTINUE) = (print "continue ;\n")

and print_pstmt (Ast.printing (x)) = (print "print( "; print_exp_list x; print");\n")

and print_exp_list ([]) = ()
  | print_exp_list ([x]) = (print_simple_exp x)
  | print_exp_list (x::xs) = (print_simple_exp x; print ",";print_exp_list xs)
 
(* ================================== expression ==================================== *)
and print_exp   (Ast.assign (x, y))      = (                 print_mutable (x) ; 
                                                             print_yellow " = " ; 
                                                             print_exp (y);
                                                             TypeCheck.typecheck_mutable_and_exp(x, y)
                                                             )


    | print_exp (Ast.assignPlus (x, y))  = (                 print_mutable (x) ;
                                                              print_yellow " += " ;
                                                             print_exp (y);
                                                             TypeCheck.typecheck_mutable_and_exp(x, y)
                                                             )


    | print_exp (Ast.assignMinus (x, y)) = (                print_mutable (x) ; 
                                                            print_yellow " -= " ; 
                                                            print_exp (y);
                                                            TypeCheck.typecheck_mutable_and_exp(x, y)
                                                            )


    | print_exp (Ast.assignMul (x, y))   = (                print_mutable (x) ; 
                                                            print_yellow " *= " ; 
                                                            print_exp (y);
                                                            TypeCheck.typecheck_mutable_and_exp(x, y)
                                                            )
    

    | print_exp (Ast.assignDiv (x, y))   = (                print_mutable (x) ; 
                                                            print_yellow " /= " ; 
                                                            print_exp (y);
                                                            TypeCheck.typecheck_mutable_and_exp(x, y)
                                                            )
    

    | print_exp (Ast.increment (x))      = (               print_mutable (x);
                                                           print_yellow "++ ";
                                                           TypeCheck.typecheck_int (x)
                                                           )


    | print_exp (Ast.decrement (x))      = (               print_mutable (x); 
                                                           print_yellow "--";
                                                           TypeCheck.typecheck_int (x)
                                                           ) 


    | print_exp (Ast.simpExpression (x)) = (               print_simple_exp (x)
                                                           )


and print_simple_exp   (Ast.or (x,y))    = (               print_simple_exp (x);
                                                           print_yellow " || " ; 
                                                           print_and_exp (y);
                                                           TypeCheck.typecheck_simple_exp (x, y))

    | print_simple_exp (Ast.no_or(x))   = (                print_and_exp (x))


and print_and_exp   (Ast.andExp (x, y)) = (                print_and_exp (x) ; 
                                                           print_yellow " && " ; 
                                                           print_unary_rel_exp (y);
                                                           TypeCheck.typecheck_and_exp (x, y)) 

    | print_and_exp (Ast.uExpr (x))     = (                print_unary_rel_exp (x))


and print_unary_rel_exp   (Ast.not (x)) = (                print_yellow " !" ; 
                                                           print_unary_rel_exp (x);
                                                           TypeCheck.typecheck_unaryRel_exp (x)
                                                           )

    | print_unary_rel_exp (Ast.rExpr(x)) = (               print_rel_exp (x))


and print_rel_exp   (Ast.relExp (x,y,z)) = (               print_sum_exp (x) ;
                                                           print_relop y; 
                                                           print_sum_exp (z);
                                                           TypeCheck.typecheck_rel_exp (x, z)
                                                           )

    | print_rel_exp (Ast.noRel (x))     = (                print_sum_exp (x))


and   print_relop (Ast.LTE) = print_yellow " <= "
    | print_relop (Ast.LT)  = print_yellow " < "
    | print_relop (Ast.GTE) = print_yellow " >= "
    | print_relop (Ast.GT)  = print_yellow " > "
    | print_relop (Ast.EQ)  = print_yellow " == "
    | print_relop (Ast.NEQ) = print_yellow " != "


and print_sum_exp (Ast.sumExp (x,y,z)) = (      print_sum_exp (x) ;
                                                print_sumop y; 
                                                print_term (z);
                                                TypeCheck.typecheck_sum_exp (x, z)
                                                )

    | print_sum_exp (Ast.onlyTerm (x)) = (      print_term (x))


and print_sumop   (Ast.PLUS)  = print_yellow " + "
    | print_sumop (Ast.MINUS) = print_yellow " - "


and print_term (Ast.mulExp (x,y,z))  = (

                                        print_term (x) ; 
                                        print_mulop y;  
                                        print_unary_exp (z);
                                        TypeCheck.typecheck_term_exp (x, z))

    | print_term (Ast.nomul (x)) = (    print_unary_exp (x))


and   print_mulop (Ast.MUL) = print_yellow " * "
    | print_mulop (Ast.DIV) = print_yellow " / "
    | print_mulop (Ast.MOD) = print_yellow " % "

and print_unary_exp (Ast.uExp (x,y)) = (      print_unary_op x; 
                                              print_unary_exp (y)
                                              )

    | print_unary_exp (Ast.fac (x)) = (       print_fac (x))


and  print_unary_op (Ast.QUES)  = print_yellow " ? "


and   print_fac (Ast.mut (x))   = (print_mutable (x))
    | print_fac (Ast.immut (x)) = (print_immutable (x))



and print_mutable ( Ast.mID (x)) = (
                                        (********Check scope here*******)
                                         Scope.checkScope(Ast.mID (x));
                                         print x
                                 )

    | print_mutable ( Ast.mArray (x,y)) = (
                                        (********Check scope here*******)
                                         Scope.checkScope(x);
                                         print_mutable (x) ;
                                         print_yellow "["; 
                                         print_exp (y) ;
                                         print_yellow "]";
                                         TypeCheck.typecheck_array_arg (y))


and print_immutable   (Ast.parans (x)) = (print "( " ; print_exp (x); print " )")
    | print_immutable (Ast.imc (x))    = (print_call (x)) 
    | print_immutable (Ast.const (x))  = (print_const (x))


and print_call (Ast.callArgs (x , y)) = (  
                                         print x ; 
                                         print " (" ; 
                                         print_arg_list (y) ; 
                                         print " )";
                                         TypeCheck.typecheck_function (x, y))


and print_arg_list ([])        = ()
    | print_arg_list ([x])     = (print_exp (x) )
    | print_arg_list (x::y)    = (print_exp (x) ; print ", " ; print_arg_list y)


and print_const   (Ast.number (x))    = (print_green x)
    | print_const (Ast.charConst (x)) = (print x)
    | print_const (Ast.true_val )     = (print "true")
    | print_const (Ast.false_val )    = (print "false")
end

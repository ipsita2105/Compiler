val last_return_type = ref Types.VOID;

structure TypeCheck = struct

    structure Ast = Absyn
    
    open printColor

    exception TYPE_ERROR

    fun update_last_return_type (Ast.integer) = (last_return_type := Types.INT) 
      | update_last_return_type (Ast.boolean) = (last_return_type := Types.BOOL)
      | update_last_return_type (Ast.character) = (last_return_type := Types.CHAR)

    and update_return_type_void () = (last_return_type := Types.VOID)

    and get_exp_type (Ast.assign (x, y))        = get_exp_type (y)  
    |   get_exp_type (Ast.assignPlus(x, y))     = get_exp_type (y)   
    |   get_exp_type (Ast.assignMinus(x, y))    = get_exp_type (y)     
    |   get_exp_type (Ast.assignMul (x, y))     = get_exp_type (y)
    |   get_exp_type (Ast.assignDiv (x, y))     = get_exp_type (y) 
    |   get_exp_type (Ast.increment (x))        = Types.INT   
    |   get_exp_type (Ast.decrement (x))        = Types.INT 
    |   get_exp_type (Ast.simpExpression (x))    = get_simple_exp_type (x)

    and get_simple_exp_type (Ast.or (x,y)) = Types.BOOL
    |   get_simple_exp_type (Ast.no_or (x)) = get_and_exp_type (x)

    and get_and_exp_type (Ast.andExp (x, y)) = Types.BOOL
    |   get_and_exp_type (Ast.uExpr (x))     = get_unaryRel_exp_type (x)

    and get_unaryRel_exp_type (Ast.not (x))   = Types.BOOL
    |   get_unaryRel_exp_type (Ast.rExpr (x)) = get_relExp_type (x)

    and get_relExp_type (Ast.relExp (x, y, z)) = Types.BOOL
    |   get_relExp_type (Ast.noRel  (x)) = get_sumExp_type (x)

    and get_sumExp_type (Ast.sumExp (x, y, z)) = Types.INT
    |   get_sumExp_type (Ast.onlyTerm (x)) = get_term_type (x) 

    and get_term_type (Ast.mulExp (x, y, z)) = Types.INT
    |   get_term_type (Ast.nomul (x)) = get_unary_exp_type (x) 

    and get_unary_exp_type (Ast.uExp (x, y)) = Types.INT
    |   get_unary_exp_type (Ast.fac (x))     = get_factor_type (x)

    and get_factor_type (Ast.mut (x)) = get_mutable_type (x)
    |   get_factor_type (Ast.immut (x)) = get_immutable_type (x)
    
    and get_mutable_type (Ast.mID (m)) = (Symbol.look(!Htable, Symbol.symbol(m)))
    |   get_mutable_type (Ast.mArray (m, e)) = get_mutable_type (m)

    and get_immutable_type (Ast.parans (x)) = get_exp_type (x)
    |   get_immutable_type (Ast.imc (x))    = get_call_type (x)
    |   get_immutable_type (Ast.const (x))  = get_const_type (x)

    and get_fun_return_type (Types.FUNCTION ([], y)) =  y
    |   get_fun_return_type (Types.FUNCTION (x, y))  =  y

    and get_call_type (Ast.callArgs (x, y)) = ( (*should give return type of
                                                  function here*)
                                                let 
                                                  val temp = Symbol.look(!Htable, Symbol.symbol(x))
                                                in
                                                    (get_fun_return_type(temp))
                                                end
                                                  
                                              )

    and get_const_type (Ast.number (x))    = Types.INT
    |   get_const_type (Ast.charConst (x)) = Types.CHAR
    |   get_const_type (Ast.true_val)      = Types.BOOL
    |   get_const_type (Ast.false_val)     = Types.BOOL
 
    and typecheck_mutable_and_exp (x, y) = let
                                                val mtype = get_mutable_type (x)
                                                val etype = get_exp_type (y)

                                           in
                                                if (mtype = etype) then ()
                                                else(    
                                                        print_red ("\nlhs type ");
                                                        print_yellow (Types.type_to_string (mtype));
                                                        print_red ("\nand rhs type ");
                                                        print_yellow (Types.type_to_string (etype));
                                                        print_red ("\ndo not match\n");
                                                        raise TYPE_ERROR
                                                )
                                           end
    and typecheck_int (m) = let 
                                val temp_type = get_mutable_type (m)
                            in
                                if(temp_type = Types.INT) then ()
                                else (
                                    print_red ("\nrhs is of type ");
                                    print_yellow (Types.type_to_string (temp_type));
                                    print_red ("should be ");
                                    print_yellow ("int ");
                                    print_red ("type\n");
                                    raise TYPE_ERROR
                                )
                            end

    and typecheck_val_init (t, y) = let
                                        val temp_type = Semantic.get_type(t)
                                        val exp_type = get_simple_exp_type (y)
                                    in
                                        if(exp_type = temp_type) then ()

                                        else(

                                    print_red ("\nrhs is of type ");
                                    print_yellow (Types.type_to_string (exp_type));
                                    print_red ("should be ");
                                    print_yellow (Types.type_to_string (temp_type));
                                    print_red ("type\n");
                                    raise TYPE_ERROR

                                        )
                                    end

    and typecheck_simple_exp (x, y) = let
                                        val a = get_simple_exp_type (x) 
                                        val b = get_and_exp_type (y) 
                                      in
                                            if(a <> Types.BOOL orelse b <> Types.BOOL) then (
                                                print_red ("\nrhs should be of only");
                                                print_yellow (" bool ");
                                                print_red ("type\n");
                                                raise TYPE_ERROR
                                            )
                                            else()

                                      end
    and typecheck_and_exp (x, y) = let
                                        val a = get_and_exp_type (x)
                                        val b = get_unaryRel_exp_type (y)
                                   in
                                        if(a <> Types.BOOL orelse b <> Types.BOOL) then (
                                                print_red ("\nrhs should be of only");
                                                print_yellow (" bool ");
                                                print_red ("type\n");
                                                raise TYPE_ERROR
                                            )
                                        else()
                                   end   

    and typecheck_unaryRel_exp (x)  = let    
                                        val a = get_unaryRel_exp_type (x)  
                                      in

                                        if(a <> Types.BOOL) then (
                                                print_red ("\nrhs should be of only");
                                                print_yellow (" bool ");
                                                print_red ("type\n");
                                                raise TYPE_ERROR
                                            )

                                        else()
                                      end
    and typecheck_rel_exp (x, y)  = let
                                        val a = get_sumExp_type (x) 
                                        val b = get_sumExp_type (y) 
                                    in
                                        if (a <> b) then (
                                             print_red ("\ncomparision type ");
                                             print_yellow (Types.type_to_string (a));
                                             print_red (" and ");
                                             print_yellow (Types.type_to_string (b));
                                             print_red (" do not match\n");
                                             raise TYPE_ERROR
                                        )
                                        else ()

                                    end
    and typecheck_sum_exp (x, y) = let 
                                        val a = get_sumExp_type (x)
                                        val b = get_term_type (y)
                                    in  
                                        if (a <> Types.INT orelse b <> Types.INT) then(

                                            print_red ("\nexpected ");
                                            print_yellow ("int ");
                                            print_red ("type\n");
                                            raise TYPE_ERROR
                                        )
                                        else ()
                                    end
                                    
    and typecheck_term_exp (x, y) = let 
                                        val a = get_term_type (x)
                                        val b = get_unary_exp_type (y)
                                    in  
                                        if (a <> Types.INT orelse b <> Types.INT) then(

                                            print_red ("\nexpected ");
                                            print_yellow ("int ");
                                            print_red ("type\n");
                                            raise TYPE_ERROR
                                        )
                                        else ()
                                    end
    
    and typecheck_unary_exp (x)  = let    
                                        val a = get_unary_exp_type (x)  
                                      in

                                        if(a <> Types.INT) then (
                                                print_red ("\nrhs should be of only");
                                                print_yellow (" int ");
                                                print_red ("type\n");
                                                raise TYPE_ERROR
                                            )

                                        else()
                                      end
    and typecheck_array_arg (x)  = let    
                                        val a = get_exp_type (x)  
                                      in

                                        if(a <> Types.INT) then (
                                                print_red ("\narray size should be of only");
                                                print_yellow (" int ");
                                                print_red ("type\n");
                                                raise TYPE_ERROR
                                            )

                                        else()
                                      end

    and get_fun_param_type (Types.FUNCTION ([], y)) =  []
    |   get_fun_param_type (Types.FUNCTION (x, y))  =  x

    and typecheck_function (x, y) = let

                                        val plist = get_fun_param_type(Symbol.look(!Htable, Symbol.symbol(x)))

                                        fun check_param_type (p, ptype) = 
                                                                    let
                                                                        val argtype = get_exp_type (p)
                                                                    in
                                                                        if (argtype = ptype) then ()
                                                                        else (
                                                                            print_red ("\nargument type is ");
                                                                            print_yellow (Types.type_to_string (argtype));
                                                                            print_red ("expected ");
                                                                            print_yellow (Types.type_to_string (ptype));
                                                                            raise TYPE_ERROR
                                                                        )

                                                                    end
                                    in
                                        (ListPair.map check_param_type (y, plist); ()) 

                                    end

    and typecheck_if (x)  = let    
                                        val a = get_simple_exp_type (x)  
                                      in

                                        if(a <> Types.BOOL) then (
                                                print_red ("\nif condition should be of only");
                                                print_yellow (" bool ");
                                                print_red ("type\n");
                                                raise TYPE_ERROR
                                            )

                                        else()
                                      end
    and typecheck_while (x)  = let    
                                        val a = get_simple_exp_type (x)  
                                      in

                                        if(a <> Types.BOOL) then (
                                                print_red ("\nwhile condition should be of only");
                                                print_yellow (" bool ");
                                                print_red ("type\n");
                                                raise TYPE_ERROR
                                            )

                                        else()
                                      end

    and check_void_return_type () = if ((!last_return_type) <> Types.VOID ) then(
   
                                        print_red ("expected ");
                                        print_yellow ("void ");
                                        print_red ("type");
                                        raise TYPE_ERROR
                                   )

                                else ()


    and check_return_type (x) =  let
                                        val e_type = get_exp_type (x)
                                in
                                        if (e_type <> (!last_return_type))then(
                                        
                                           print_red ("Return type is ");
                                           print_yellow (Types.type_to_string (e_type));
                                           print_red (" expected ");
                                           print_yellow (Types.type_to_string (!last_return_type));
                                           raise TYPE_ERROR
                                        )

                                        else ()
                                end      
                                
                                                                

                               
end

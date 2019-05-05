structure Translate =
struct

open printColor;

(*This is for the indentation*)
(*This set of function is used for printing the indenation correctly*)
val indent = ref 0
val n = ref 0
fun print_tabs () = if !n = 0 then ( print("")) else (n := !n - 1 ; print ("\t") ; print_tabs ());
fun print_tabs_real () = (n := !indent ; print_tabs () )

(* =============================== main program ======================================= *)
fun compileProgram (Ast.declL(x)) = printDeclarationList x

and printDeclarationList (Ast.declList (x,y)) = (printDeclarationList x ; printDeclaration y)
    | printDeclarationList (Ast.singleDecl(x)) = (printDeclaration x)

and printDeclaration (Ast.variableDeclaration (x)) = (printVarDeclaration x)
    | printDeclaration (Ast.functionDeclaration (x)) = (printFunDeclaration x)
    | printDeclaration (Ast.recordDeclaration (x)) = (printRecordDeclaration x)

(* ==================================== record declaration ================================= *)
and printRecordDeclaration (Ast.recordID (x,y)) = ( 
                                                    print_red "record ";
                                                     print_default x;
                                                     print_yellow " {\n";
                                                     indent := !indent + 1;
                                                    print_tabs_real();
                                                    printLocalDeclarations y;
                                                    indent := !indent - 1 ;
                                                    print_tabs_real ();
                                                    print_yellow "}\n"
                                                    )

(* ===================================== variable declaration ================================= *)
and printVarDeclaration (Ast.vDecl(x,y)) = (print_tabs_real () ; printTypeSpecifier x ; printVarDeclList y ; print ";\n")

and printScopedVarDeclaration (Ast.sDecl (x,y)) = (print_tabs_real(); printScopedTypeSpecifier x ; printVarDeclList y ; print ";\n")

and printVarDeclList (Ast.vList (x,y)) = (printVarDeclList x ; print " , " ; printVarDeclInitialize y)
    | printVarDeclList (Ast.vSingleDecl (x)) = (printVarDeclInitialize x)

and printVarDeclInitialize (Ast.declarationOnlyID (x)) = (printVarDeclID x)
    | printVarDeclInitialize (Ast.declarationAssignment (x,y)) = (printVarDeclID x ; print_default " : " ; printSimpleExpression y)

and printVarDeclID (Ast.vID (x)) = (print_default x)
    | printVarDeclID (Ast.arrayLike (x,y)) = (  
                                            print_default x;
                                            print "[";
                                            print_default y;
                                            print "]"
                                            )

and printScopedTypeSpecifier (Ast.staticType (x)) = (print_red "static " ; printTypeSpecifier x )
    | printScopedTypeSpecifier (Ast.simpleType (x)) = (printTypeSpecifier x)

and printTypeSpecifier (Ast.ret (x)) = (printReturnTypeSpecifier x)

and printReturnTypeSpecifier (Ast.integer) = (print_cyan "int ")
    | printReturnTypeSpecifier (Ast.boolean) = (print_cyan "bool ")
    | printReturnTypeSpecifier (Ast.character) = (print_cyan "char ")


(* ===================================== function declaration ============================== *)

and printFunDeclaration (Ast.functionReturn (x,y,z,w)) = (
                                                            printTypeSpecifier x;
                                                            print "" ; print_magneta y ; print " ";
                                                            print "( ";
                                                            printParams z;
                                                            print " )";
                                                            printStatement w
                                                         )
    | printFunDeclaration (Ast.functionVoid (y,z,w)) = (
                                                            print_red "void";
                                                            print "" ; print_magneta y ; print " ";
                                                            print "( ";
                                                            printParams z;
                                                            print " )";
                                                            printStatement w
                                                         )

and printParams (Ast.parameterList (x)) = (printParamList x)
    | printParams (Ast.emptyParameter) = ()

and printParamList (Ast.pList (x,y))  = (printParamList x ; print ", " ; printParamTypeList y)
    | printParamList (Ast.singleParam (x)) = (printParamTypeList x)

and printParamTypeList (Ast.parameter(x,y)) = (printTypeSpecifier x ; printParamIDList y)

and printParamIDList (Ast.listOfID (x ,y))  = (printParamIDList x ; print ", " ; printParamID y)
    | printParamIDList (Ast.singleIDParameter (x)) = (printParamID x)

and printParamID (Ast.normalID(x))   = (print_default x ; print " ")
    | printParamID (Ast.arrayID (x)) = (print_default x ; print " [] " )

(* ============================= for the statements ============================ *)
and printStatement (Ast.eStatement(x)) = (print_tabs_real () ; printExpressionStmt x)
   | printStatement (Ast.cStatement(x)) = (print_tabs_real () ; printCompoundStmt x)
   | printStatement (Ast.sStatement(x)) = (print_tabs_real () ; printSelectionStmt x)
   | printStatement (Ast.iStatement(x)) = (print_tabs_real () ; printIterationStmt x)
   | printStatement (Ast.rStatement(x)) = (print_tabs_real () ; printReturnStmt x)
   | printStatement (Ast.bStatement(x)) = (print_tabs_real () ; printBreakStmt x)
   | printStatement (Ast.conStatement(x)) = (print_tabs_real () ; printContinueStmt x)

and printCompoundStmt (Ast.statementWithBrace (x,y)) = (
                                                        print_yellow "{\n";
                                                        indent := !indent + 1;
                                                        printLocalDeclarations x;
                                                        printStatementList y;
                                                        print_tabs_real();
                                                        print_yellow "}";
                                                        print "\n";
                                                        indent := !indent - 1
                                                        ) 

and printLocalDeclarations (Ast.declIn(x,y)) = (
                                                printLocalDeclarations x;
                                                printScopedVarDeclaration y
                                                )
    | printLocalDeclarations (Ast.emptyDeclIn ) = ()

and printStatementList (Ast.listOfStatements (x,y)) = (printStatementList x ; printStatement y)
    | printStatementList (Ast.emptyListStatement) = ()

and printExpressionStmt (Ast.basicExpression (x)) = (printExpression x ; print ";\n") 
    | printExpressionStmt (Ast.semicolon) = (print ";\n")

and printSelectionStmt (Ast.IF (x,y)) = (
                                            print_red "if ";
                                            print_yellow "(";
                                            printSimpleExpression x;
                                            print_yellow " )";
                                            printStatement y
                                        )
    | printSelectionStmt (Ast.IF_ELSE (x,y,z)) = (
                                                    print_red "if ";
                                                    print_yellow "( ";
                                                    printSimpleExpression x;
                                                    print_yellow " )";
                                                    printStatement y;
                                                    print_tabs_real();
                                                    print_red "else";
                                                    printStatement z
                                                )

and printIterationStmt (Ast.WHILE (x,y)) = (
                                                print_red "while ";
                                                print_yellow "( ";
                                                printSimpleExpression x;
                                                print_yellow " )";
                                                printStatement y
                                           )

and printReturnStmt (Ast.returnNoValue ) = (print_tabs_real () ; print_red " return " ; print_yellow ";\n")
    | printReturnStmt (Ast.returnValue (x)) = (print_tabs_real(); print_red " return " ; printExpression x ; print_yellow ";\n" )

and printBreakStmt (Ast.BREAK) = (print "break ;\n") 
and printContinueStmt (Ast.CONTINUE) = (print "continue ;\n")
 
(* ================================== expression ==================================== *)
and printExpression (Ast.assign (x, y)) = (printMutable x ; print_yellow " = " ; printExpression y)
    | printExpression (Ast.assignPlus (x, y)) = (printMutable x ; print_yellow " += " ; printExpression y)
    | printExpression (Ast.assignMinus (x, y)) = (printMutable x ; print_yellow " -= " ; printExpression y)
    | printExpression (Ast.assignMult (x, y)) = (printMutable x ; print_yellow " *= " ; printExpression y)
    | printExpression (Ast.assignDiv (x, y)) = (printMutable x ; print_yellow " /= " ; printExpression y)
    | printExpression (Ast.increment (x)) = (printMutable x ; print_yellow "++ ")
    | printExpression (Ast.decrement (x)) = (printMutable x ; print_yellow "--") 
    | printExpression (Ast.plainExpression (x)) = (printSimpleExpression x)

and printSimpleExpression (Ast.or (x,y)) = (printSimpleExpression x ; print_yellow " || " ; printAndExpression y)
    | printSimpleExpression (Ast.noOr(x)) = (printAndExpression x)

and printAndExpression (Ast.simpleAnd (x , y)) = ( printAndExpression x ; print_yellow " && " ; printUnaryRelExpression y) 
    | printAndExpression (Ast.uExpr (x)) = (printUnaryRelExpression x)

and printUnaryRelExpression (Ast.not (x)) = (print_yellow " !" ; printUnaryRelExpression x )
    | printUnaryRelExpression (Ast.rExpr (x)) = (printRelExpression x)

and printRelExpression (Ast.relExp (x,y,z)) = (printSumExpression x ; printRelop y; printSumExpression z)
    | printRelExpression (Ast.noRel (x)) = (printSumExpression x)

and printRelop (Ast.LTE) = print_yellow " <= "
    | printRelop (Ast.LT) = print_yellow " < "
    | printRelop (Ast.GTE) = print_yellow " >= "
    | printRelop (Ast.GT) = print_yellow " > "
    | printRelop (Ast.EQ) = print_yellow " == "
    | printRelop (Ast.NEQ) = print_yellow " != "

and printSumExpression (Ast.sumExp (x,y,z)) = (printSumExpression x ; printSumOp y; printTerm z )
    | printSumExpression (Ast.noSum (x)) = (printTerm x)

and printSumOp (Ast.PLUS) = print_yellow " + "
    | printSumOp (Ast.MINUS) = print_yellow " - "

and printTerm (Ast.multExp (x,y,z)) = (printTerm x ; printMulOp y;  printUnaryExpression z )
    | printTerm (Ast.noMult (x)) = (printUnaryExpression x)

and printMulOp (Ast.MULT) = print_yellow " * "
    | printMulOp (Ast.DIV) = print_yellow " / "
    | printMulOp (Ast.MOD) = print_yellow " % "

and printUnaryExpression (Ast.uExp (x,y)) = (printUnaryOp x; printUnaryExpression y)
    | printUnaryExpression (Ast.noUnary (x)) = (printFactor x)

and printUnaryOp (Ast.STAR) = print_yellow " ** "
    | printUnaryOp (Ast.DASH) = print_yellow " $$ "
    | printUnaryOp (Ast.QUES) = print_yellow " ? "

and printFactor (Ast.mut (x)) = (printMutable x)
    | printFactor (Ast.immut (x)) = (printImmutable x)


and printMutable (Ast.mID (x)) = (print x)
    | printMutable (Ast.mArray (x,y)) = (printMutable x ; print_yellow "["; printExpression y ; print_yellow "]")
    | printMutable (Ast.mRecord (x,y)) = (printMutable x ; print_yellow "." ; print y)

and printImmutable (Ast.paranthesis (x)) = (print "( " ; printExpression x; print " )")
    | printImmutable (Ast.c (x)) =         (printCall x) 
    | printImmutable (Ast.const (x)) = (printConstant x)

and printCall (Ast.callArgs (x , y)) = (print x ; print " (" ; printArgs y ; print " )")

and printArgs (Ast.aList (x)) = (printArgList x)
    | printArgs (Ast.emptyArg) = ()

and printArgList (Ast.aaList (x,y)) = (printArgList x ; print ", " ; printExpression y)
    | printArgList (Ast.oneArg (x)) = (printExpression x) 

and printConstant (Ast.number (x)) = (print_green x)
    | printConstant (Ast.charConst (x)) = (print x)
    | printConstant (Ast.trueValue ) = (print "true")
    | printConstant (Ast.falseValue ) = (print "false")

end

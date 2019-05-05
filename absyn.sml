structure Absyn = struct
        

        type pos = int

        datatype program =  declL of declaration list
                            (*declL of declarationList*)

       (* and declarationList = declList of declaration list 
                            (*declList   of (declarationList * declaration) 
                            | oneDecl of declaration*)
        *)
        and declaration = variableDeclaration of varDeclaration
                        | functionDeclaration  of funDeclaration
                  (*      | recordDeclaration    of recDeclaration*)

        (*******record = recordID {localDeclaration}******)

        (*and recDeclaration = recordID of string*localDeclarations*)

        (******************variable declaration************************************)

        and varDeclaration  = vDecl of (typeSpecifier * varDeclInitialize list)
        
        (*and scopedVarDeclaration = sDecl of (scopedTypeSpecifier* varDeclList)*)

        (*and varDeclList          = vList of (varDeclList*varDeclInitialize) 
                                | vSingle of varDeclInitialize*)
        
        and varDeclInitialize    = declOnlyID of varDeclID
                                (* = varDeclID: simpleExpression*)
                                | declAssign of (varDeclID*simpleExpression) 
        
        and varDeclID            = vID of string
                                (*ID [NUMCONST]*)
                                | arrayDecl of (string*string)
        
        (*and scopedTypeSpecifier = staticType of typeSpecifier
                                | onlyType   of typeSpecifier*)

        and typeSpecifier       =  integer
                                |  boolean
                                |  character

       (******************funtion declaration************************************)
                             (*typeSpecifier ID (params) statement*)
        and funDeclaration = funReturn of (typeSpecifier*string*paramTypeList list*compoundStmt)
                           | funVoid   of (string*paramTypeList list*compoundStmt)

        (*and params         = parameterList of paramTypeList list 
                           (*parameterList of paramList*)
                           | emptyParam
        *)
        (*and paramList      = paramTypeList list 
                           (*pList of (paramList*paramTypeList)*)
                           (*| oneParam of (paramTypeList)*)
        *)
        and paramTypeList  = parameter of (typeSpecifier*paramID)

        (*and paramIDList    = listofID of (paramIDList*paramID)
                           | oneIDParameter of (paramID)*)
        
                           (*ID | ID []*)
        and paramID        = justID of string
                           | arrayID of string

        (******************statements************************************)
        and statement      = eStatement   of expressionStmt
                           | cStatement   of compoundStmt
                           | sStatement   of selectionStmt
                           | iStatement   of iterationStmt
                           | rStatement   of returnStmt
                           | bStatement   of breakStmt
                           | conStatement of continueStmt
                           | printStatement of printStmt

                        (*{localDeclarations statementList}*)
        and compoundStmt      = braceStmt of (localDeclarations*statement list)

        and localDeclarations = ldecls of (localDeclarations*varDeclaration)
                              | emptyldecls

        (*and statementList     = listStatements of (statementList*statement)
                              | emptyStmtList*)

        and expressionStmt    = sExpression of (expression)
                              | semicolon

                              (*if (simpleExpression) statement
                              | if (simpleExpression) statement else statement*)
        and selectionStmt     = IF of (simpleExpression*statement)
                              | IFELSE of (simpleExpression*statement*statement)

                              (*while (simpleExpression) statement*)
        and iterationStmt     = WHILE of (simpleExpression*statement) 
        and returnStmt        = voidReturn
                              | returnValue of (expression)
        and breakStmt         = BREAK
        and continueStmt      = CONTINUE

        and printStmt         = printing of (simpleExpression list) 


       (*********************expressoind*************************************)
        and expression = assign      of (mutable*expression)
                       | assignPlus  of (mutable*expression)
                       | assignMinus of (mutable*expression)
                       | assignMul   of (mutable*expression)
                       | assignDiv   of (mutable*expression)
                       | increment   of  mutable
                       | decrement   of  mutable
                       | simpExpression of simpleExpression

        and simpleExpression   = or    of (simpleExpression*andExpression)
                               | no_or of andExpression

        and andExpression      = andExp of (andExpression*unaryRelExpression)
                               | uExpr     of unaryRelExpression

        and unaryRelExpression = not   of unaryRelExpression
                               | rExpr of relExpression

        and relExpression      = relExp of (sumExpression*relop*sumExpression)
                               | noRel  of (sumExpression)

        and relop              = LTE | LT | GT | GTE | EQ | NEQ

        and sumExpression      = sumExp of (sumExpression*sumOp*term) 
                               | onlyTerm  of (term) 
        
        and sumOp              = PLUS | MINUS

        and term               = mulExp of (term*mulOp*unaryExpression)
                               | nomul  of unaryExpression

        and mulOp              = MUL | DIV | MOD

        and unaryExpression    = uExp    of (unaryOp*unaryExpression)
                               | fac of (factor)

        and unaryOp            = QUES

        and factor             = mut   of (mutable)
                               | immut of (immutable)
        
        and mutable            = mID     of string
                               | mArray  of (mutable*expression)  
        (*                       | mRecord of (mutable*string)*)
        
        and immutable          = parans of expression
                               | imc of call
                               | const of constant

        and call               = callArgs of (string*expression list)

        (*and args               = aList of expression list 

        and args               = aList of argList
                               | emptyArg*)

        (*and argList            = arg_list of (argList*expression)
                               | oneArg of (expression) 
                               *)
        
        and constant           = number of string
                               | charConst of string
                               | true_val
                               | false_val
end

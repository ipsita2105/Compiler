structure Stack = 
  struct

    type 'a stack = 'a list
    type 'a ref_stack = 'a list ref
    exception Empty

    val empty = []
 
    fun isEmpty (l:'a list): bool = (case l of
                                        [] => true
                                       | _ => false)

    fun push (x:'a, l:'a stack):'a stack = x::l

    fun pop (l:'a stack):'a stack = (case l of 
                                        [] => raise Empty
                                 | (x::xs) => xs)

    fun top (l:'a stack):'a = (case l of
                                    [] => raise Empty
                             | (x::xs) => x)

    fun map (f:'a -> 'b) (l:'a stack):'b stack = List.map f l

    fun app (f:'a -> unit) (l:'a stack):unit = List.app f l

    fun print_symbol_stack (l: Symbol.symbol stack) = let
                                        fun print_symbol (s,n) = (print(s);print(", ");print(Int.toString(n));print("\n"))
                                    in
                                        map print_symbol l
                                    end

  end

val GStack:(Symbol.symbol Stack.ref_stack) = ref Stack.empty
(*val tt = (GStack := Stack.push(x, !GStack))
val tt = (GStack := Stack.push(y, !GStack))
val tt = (print("*********Stack**************\n"))
val tt = Stack.isEmpty (!GStack)
val tt = Stack.print_symbol_stack(!GStack)
*)
val marker_symbol:Symbol.symbol = ("$", 0)

structure Scope = 
struct

    structure Ast = Absyn

    open printColor

    exception NOT_IN_SCOPE

    fun beginScope () =
                        (GStack := Stack.push(marker_symbol, !GStack))

    fun scopeInsert ((s, n), a) = let
                                val tt = (GStack := Stack.push((s, n), !GStack))
                                val xx = (Htable := Symbol.enter((!Htable), (s, n), a))
                               in ()
                               end

    fun scopeInsertFunction (plist:(Ast.paramTypeList list)) = 
    
                                 let
                                        fun add_paramter_to_scope (Ast.parameter (t, v)) = case (t, v) of

                                                                 (Ast.integer  , Ast.justID v)    => (scopeInsert(Symbol.symbol (v), Types.INT))
                                                               | (Ast.character, Ast.justID v)    => (scopeInsert(Symbol.symbol (v), Types.CHAR))
                                                               | (Ast.boolean,   Ast.justID v)    => (scopeInsert(Symbol.symbol (v), Types.BOOL))
                                                               | (Ast.integer,   Ast.arrayID v)   => (scopeInsert(Symbol.symbol (v), Types.INTARRAY))
                                                               | (Ast.character, Ast.arrayID v)   => (scopeInsert(Symbol.symbol (v), Types.CHARARRAY))
                                                               | (Ast.boolean, Ast.arrayID v)     => (scopeInsert(Symbol.symbol (v), Types.BOOLARRAY))

                                
                                 in
                                        map add_paramter_to_scope plist
                                 end

    fun endScope() = let
                        val (s, n) = Stack.top(!GStack) 
                    in
                        if(s = "$") then ( GStack := (Stack.pop(!GStack)) )
                        else(
                                (*pop from stack and hash table*)
                                Htable := Symbol.remove((!Htable), (s,n));
                                GStack := Stack.pop(!GStack);
                                endScope()
                        )
                    end

    fun checkScope(Ast.mID (x)) = let
                                        val (s, n) = Symbol.symbol(x)
      
                                    in
                                        if ( IntBinaryMap.inDomain(!Htable, n)) then ()
                                         else (   
                                                 (print_red ("***variable "); print_red(s); print_red(" not in scope***\n"));
                                                 raise NOT_IN_SCOPE      
                                         )

                                    end

end

structure Semantic = 
struct

  val main_fun = ref 0

  structure Ast = Absyn
    
  exception NO_MAIN
  exception MORE_THAN_ONE_MAIN

  fun get_type Ast.integer   = (Types.INT)
    | get_type Ast.boolean   = (Types.BOOL)
    | get_type Ast.character = (Types.CHAR)

  fun get_array_type Ast.integer   = (Types.INTARRAY)
    | get_array_type Ast.boolean   = (Types.BOOLARRAY)
    | get_array_type Ast.character = (Types.CHARARRAY)

  (*Ast.parameter is of form
  * (x, y) where x is typeSpecifier and y is param name
  * y can be justID or arrayID
  * that is needed for type*)

  (*fun get_param_type (t:Ast.typeSpecifier, v:Ast.paramID) = case (t, v) of*)
  fun get_param_type (Ast.parameter (t, v)) = case (t, v) of
                                                                 (Ast.integer  , Ast.justID v)  => (Types.INT)
                                                               | (Ast.character, Ast.justID v) => (Types.CHAR)
                                                               | (Ast.boolean, Ast.justID v) => (Types.BOOL)
                                                               | (Ast.integer, Ast.arrayID v) => (Types.INTARRAY)
                                                               | (Ast.character, Ast.arrayID v) => (Types.CHARARRAY)
                                                               | (Ast.boolean, Ast.arrayID v) => (Types.BOOLARRAY)

  fun get_fun_type (r:Ast.typeSpecifier, (plist:(Ast.paramTypeList list))) =
 
                                    let 

                                      (*val newlist = [Types.VOID]*)
                                      val newlist = []
                                      fun add_in_list pelement = newlist@[get_param_type (pelement)]

                                    in
                                      
                                      (Types.FUNCTION (map get_param_type plist, get_type r))

                                    end
  
  fun get_voidfun_type (plist:(Ast.paramTypeList list)) =
 
                                    let 

                                      val newlist = []
                                      fun add_in_list pelement = newlist@[get_param_type (pelement)]

                                    in
                                      map add_in_list plist;
                                      (Types.FUNCTION (map get_param_type plist, Types.VOID))

                                    end

  fun found_main (x) =     if(x = "main") then (main_fun := !main_fun + 1)
                           else () 
                                    
  fun check_for_main () = let
                                val times_main = (!main_fun)
                          in

                                if (times_main = 0) then (
                                        print ("*******NO MAIN FUNCTION************"); raise NO_MAIN
                                
                                )else (
                                
                                        if (times_main = 1) then ()
                                        else(
                                                raise MORE_THAN_ONE_MAIN
                                        )
                                
                                
                                )

                          end    


end

(*signature SYMBOL = 
sig
  eqtype symbol
  val symbol: string->symbol
  val name: symbol->string

   type 'a table
   type 'a ref_table
   val empty: 'a table
   val enter: 'a table*symbol*'a -> 'a table
   val look : 'a table*symbol -> 'a option
end
*)

structure Symbol = 
struct
        type symbol = string*int

        exception Symbol
        val nextsym = ref 0

        (*Internal hashtable keep track of symbols so far and give int 
        * accordingly*)
        val hashtable : (string, int) HashTable.hash_table = 
                HashTable.mkTable(HashString.hashString, op = ) (128, Symbol)

        (*take a string and return symbol, count for it*)
        fun symbol name = 
                case HashTable.find hashtable name
                  of SOME i => (name, i)
                   | NONE   => let val i = !nextsym
                                in nextsym := i+1;
                                        HashTable.insert hashtable (name, i);
                                        (name, i)
                                end

        fun name(s, n) = s

        type 'a table = 'a list IntBinaryMap.map
        type 'a ref_table = 'a list IntBinaryMap.map ref

        (*Use a IntBinaryMap to store these pairs*)
        val empty  =  IntBinaryMap.empty

        fun enter(t, (s, n), a) = (*IntBinaryMap.insert(t,n,a) *)
                                                      if (IntBinaryMap.inDomain(t,n))
                                                      then
                                                        let
                                                          val old = IntBinaryMap.lookup(t, n)
                                                        in
                                                          (IntBinaryMap.insert(t, n, a::old))
                                                        end
                                                      else
                                                        (IntBinaryMap.insert(t, n, [a]))
        
        fun look(t, (s,n ))                 =     let 
                                                    val v = IntBinaryMap.lookup(t,n)
                                                  in
                                                    (List.hd v)
                                                  end
        
        fun remove(t ,(s, n))           =  if (IntBinaryMap.inDomain(t, n))
                                                then
                                                  let
                                                    val old = IntBinaryMap.lookup(t,n)
                                                  in
                                                    if(List.null(List.tl old)) then
                                                      let 
                                                        val (x,y) = IntBinaryMap.remove(t,n)
                                                      in
                                                        (x)
                                                      end
                                                    else
                                                      (IntBinaryMap.insert(t,n,List.tl old))
                                                  end
                                                else
                                                  (t)

        fun print_hash_table htable = let
                                fun print_hash_element (n, l) =

                                                          let 
                                                            fun print_rhs_element (r:Types.ty) = case r of
                                                                                                Types.INT  => (print("INT "))
                                                                                              | Types.BOOL => (print("BOOL "))
                                                                                              | Types.CHAR => (print("CHAR "))
                                                                                              | Types.VOID => (print("VOID "))
                                                                                              | Types.INTARRAY => (print("INTARRAY "))
                                                                                              | Types.BOOLARRAY => (print("BOOLARRAY "))
                                                                                              | Types.CHARARRAY=> (print("CHARARRAY "))
                                                                                              | Types.FUNCTION(x,y) =>
                                                                                                  (print("FUNCTION ");Types.funtype_to_string(x,y))
                                                                                              
                                                                                              
                                                          in
                                                          (print(Int.toString(n));print(": "); map print_rhs_element l;print("\n"))
                                                          end
  
                              in
                                map print_hash_element (IntBinaryMap.listItemsi htable)
                              end
          
end


val Htable:(Types.ty Symbol.ref_table) = ref IntBinaryMap.empty

(*val x:Symbol.symbol = Symbol.symbol ("A")
val y:Symbol.symbol = Symbol.symbol ("B")
val z:Symbol.symbol = Symbol.symbol ("A")

val tt = (Htable := Symbol.enter((!Htable), x, 11)) 
val tt = (Htable := Symbol.enter((!Htable), y, 22)) 
val tt = (Htable := Symbol.enter((!Htable), z, 33)) 
val tt = Symbol.print_hash_table (!Htable)                     

val tt = (Htable := Symbol.remove((!Htable), x))*)


val nullable = ref AtomSet.empty

(*This corresponds to one production of a symbol*)
(*A single production is a list of atoms*)
fun is_list_nullable [] = true
  | is_list_nullable (x::xs) =  (AtomSet.member ((!nullable), x)) andalso (is_list_nullable xs)

 
(*A symbol is nullable if anyone of its production is nullable*)
(*here s is the symbol along with its productions*)
(*Productions are list of list of atoms*)
fun is_symbol_nullable  []    = false
  | is_symbol_nullable (x::xs) = (is_list_nullable x) orelse (is_symbol_nullable xs)


fun add_if_null (true, s)  = (nullable := AtomSet.add ((!nullable), s))
  | add_if_null (false,s)  = ()


(*Now i need a function that adds nullable symbols to the set*)
fun is_prodn_nullable (s,p) =  let
                                val x = RHSSet.listItems p
                                val is_null = is_symbol_nullable  x
                           in
                                add_if_null (is_null, s)
                           end 

(*For just calling on all rules*)
fun map_all_rules () = map is_prodn_nullable (AtomMap.listItemsi (!grules)) 

fun all_nullable_found true  = ()
  | all_nullable_found false = (find_nullable ())

and  find_nullable () = let
                         val nullable_old = (!nullable)
                         val x = map_all_rules ()
                         val b = AtomSet.equal (nullable_old, (!nullable))

                        in 
                          all_nullable_found b
                       end

val n = find_nullable ()
val p = (print("***Nullable symbols are***\n"); print_set (!nullable); print("\n"))

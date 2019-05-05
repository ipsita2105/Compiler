type x = AtomSet.set AtomMap.map

(*Map from symbol to set of firsts*)
val first:x ref = ref AtomMap.empty

(*First initialize the map*)
(*Add all symbols with empty sets*)
fun init_symbols s = (first := AtomMap.insert (!first, s, AtomSet.empty))
fun map_init_symbols x = map init_symbols (AtomSet.listItems x)
val ss = map_init_symbols (!gsymbols)

(*initialize with tokens also*)
fun init_tokens t = (first := AtomMap.insert (!first, t, AtomSet.singleton t))
fun map_init_tokens x = map init_tokens (AtomSet.listItems x)
val tt = map_init_tokens (!gtokens)

(*function to insert in map*)
(*This function adds first to already computed first*)
fun insert_in_first (s, fset) = let 
                                        val old_set = AtomMap.lookup (!first, s)
                                in
                                        (first := AtomMap.insert(!first, s, AtomSet.union(old_set, fset)))
                                end

(*s is symbol and the other input one of the rules
* i.e. a list of Atoms*)
fun find_first (s, x::xs) = if (AtomSet.member (!gtokens, x)) then (insert_in_first (s, AtomSet.singleton x))

                           else
                                if (AtomSet.member ((!nullable), x)) then
                                 (insert_in_first (s, AtomMap.lookup (!first, x)); (find_first (s, xs)))

                                else
                                (insert_in_first(s, AtomMap.lookup (!first, x)))

  | find_first (s, [])   =  ()                        

(*find first for all productions of a symbol therefore a map*)
fun map_on_productions (f , s, (x::xs)) = [f (s, x)]@(map_on_productions (f, s, xs))
  | map_on_productions (f , s, [])      = []

(*Input is symbol and all its productions
* therefore map on all productions one by one*)  
fun first_productions (s, p) = let 
                                   val l = RHSSet.listItems p (*List of list of atoms*)
                               in
                                   map_on_productions (find_first, s, l)
                               end

fun first_all_rules () = map first_productions (AtomMap.listItemsi (!grules))

(*Do for num of symbols time*)
fun find_all_first n = if n > 1 then (first_all_rules (); find_all_first (n-1))

                       else  ()
                             
val r = find_all_first (AtomSet.numItems (!gsymbols))

(*Function to print first*)
fun print_first_list (s, fset) = (print_Atom s; print(" -> "); print_set fset; print("\n"))

fun print_first () = map print_first_list (AtomMap.listItemsi (!first))

val tt = print_first ()

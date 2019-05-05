type x = AtomSet.set AtomMap.map

(*Map from symbol to set of Atoms*)
val follow:x ref = ref AtomMap.empty

(*Initialize the map*)
fun init_follow_symbols s = (follow := AtomMap.insert (!follow, s, AtomSet.empty))
fun map_init_follow_symbols x = map init_follow_symbols (AtomSet.listItems x)
val ss = map_init_follow_symbols (!gsymbols)


(*Input is symbol and the set of follow elements*)
fun add_in_follow (s, fset) = let
                                val old_set = AtomMap.lookup (!follow, s)
                              in
                                (follow := AtomMap.insert(!follow, s, AtomSet.union(old_set, fset)))
                              end
fun head (x::xs) = x
  | head []    = []

fun find_first_list [] = (AtomSet.empty)
  | find_first_list (x::xs) = (if (AtomSet.member (!nullable, x)) then
                                 ( let
                                    val t1 = find_first_list xs
                                    val t2 = AtomMap.lookup ((!first) , x )
                                  in
                                    AtomSet.union (t2 , t1)
                                  end
                                )
                                else
                                 AtomMap.lookup ((!first) , x)
                              )

fun find_follow (s, x::xs) = (
                                if (AtomSet.member(!gsymbols, x)) then(


                                   if(is_list_nullable xs) then

                                     let 
                                       val s1 = AtomMap.lookup (!follow, s)
                                       val s2 = find_first_list xs
                                     in
                                       (add_in_follow(x, AtomSet.union(s1, s2)))
                                    end

                                   else
                                     (add_in_follow(x, find_first_list xs))
                                  
                                   ; find_follow (s, xs) 
                                 ) 

                                else
                            
                                find_follow(s, xs)
                           )

    | find_follow (s, []) = ()       

fun follow_production (s, x) = let
                                   val l = RHSSet.listItems x
                                   fun f x = find_follow (s, x)
                               in
                                   map f l
                               end

fun follow_all_rules () = map follow_production (AtomMap.listItemsi (!grules))

fun find_all_follow n = if n > 1 then (follow_all_rules (); find_all_follow  (n-1)) else follow_all_rules ()

val nn = find_all_follow (AtomSet.numItems (!gsymbols))

(*Function to print follow*)
fun print_follow_list (s, fset) = (print_Atom s; print(" -> "); print_set fset; print("\n"))

fun print_follow () = map print_follow_list (AtomMap.listItemsi (!follow))

val pn = print("******************FOLLOW*******************");
val ttt = print_follow ()



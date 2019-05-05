(*given a production return tokens on which it will print*)
(*input is single production rule
* ie symbol with list of list of atoms
*)

fun get_rule_token (s, l) = if (is_list_nullable l) then
                                   
                                   let
                                       val s1 = find_first_list l
                                       val s2 = AtomMap.lookup (!follow, s)
                                   in
                                       AtomSet.union(s1, s2)
                                   end

                                else

                                  find_first_list l

fun print_production (s, al) = (print_Atom s; print("->"); print_atom_list al)
                                

fun find_ll1 (s, x::xs) = (print_production(s, x::xs); 
                           print_set (get_rule_token (s, x::xs));
                           print("\n\n"))

  | find_ll1 (s, [])    = (print_production(s, []);
                           print_set(get_rule_token(s, []));
                           print("\n\n"))



fun all_production_ll (s, p) = let
                                val ll = RHSSet.listItems p
                             in
                                map_on_productions (find_ll1, s, ll)

                             end


fun ll1_all () = map all_production_ll (AtomMap.listItemsi (!grules))

val ll1 = ll1_all ();

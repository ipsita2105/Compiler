type item = { lhs: Atom.atom, before: Atom.atom list, after: Atom.atom list}

val aItem = { lhs = Atom.atom "A", before = List.map Atom.atom ["A", "a"] , after  = List.map Atom.atom ["b", "B"]}

fun print_atom2 a = (print (Atom.toString a))

fun print_atom_list2 al = map print_atom2 al

fun print_item (r:item) = let
                              val l = #lhs r
                              val b = #before r
                              val a = #after r
                          in
                              (print_atom2 l; print ("->"); print_atom_list2 (List.rev b); print ("."); print_atom_list2 a; print("\n"))
                          end

fun  compare_atom_list ([], [])       = EQUAL
   | compare_atom_list (x,  [])       = GREATER
   | compare_atom_list ([],  y)       = LESS
   | compare_atom_list (a::aas, b::bs) = case Atom.same(a, b) of 
                                            false => Atom.compare(a, b)
                                          | true  => compare_atom_list (aas, bs)

(*because we have to define set of items*)
structure ITEM_KEY: ORD_KEY = struct

        type ord_key = item

        fun compare (r1:item, r2:item) = let
                                                 val l1 = #lhs r1
                                                 val l2 = #lhs r2
                                                 val b1 = #before r1
                                                 val b2 = #before r2
                                                 val a1 = #after r1
                                                 val a2 = #after r2
                                          in
                                                case (Atom.compare(l1, l2), compare_atom_list (b1, b2), compare_atom_list (a1, a2)) of
                                                     (GREATER, _      , _) => GREATER
                                                   | (LESS   , _      , _) => LESS
                                                   | (EQUAL  , GREATER, _) => GREATER
                                                   | (EQUAL  , LESS   , _) => LESS
                                                   | (EQUAL  , EQUAL  , x) => x


                                          end
end

structure ItemSet = RedBlackSetFn (ITEM_KEY)

(*input is an item set and a symbol
* function adds rules to given set*)
fun add_same_rules (s, iset) = let
                                        (*map this on list of productions*)
                                        fun get_symbol_rule (snew, p) = 
                                        
                                                                 let
                                                                    val l = RHSSet.listItems p (*l is list of atom list*)

                                                                    (*rule_rhs is list of atom*)
                                                                    fun check_symbol_add (rule_rhs) = 
                                                                                 let
                                                                                    val x = {lhs= s, before = List.map Atom.atom [], after = rule_rhs};
                                                                                 in
                                                                                    if (Atom.same(s, snew)) then
                                                                                                (iset := ItemSet.add((!iset), x)) 
                                                                                              else ()
                                                                                 end
                                                                 in  
                                                                    (*map on rule rhs here*)
                                                                    map check_symbol_add l
                                                                 end
                                in
                                        map get_symbol_rule (AtomMap.listItemsi (!grules))

                                end


(*For given set 
for any item like A-> a.Xb in set
do add_same_rules X in that set*)

fun closure(iset) = let
                           (*input is set element that is an item*)
                           fun call_add_rule (r:item) = let
                                                             val l = #lhs r
                                                             val b = #before r
                                                             val a = #after r
                                                         in
                                                            if (List.null (a) = false) then(

                                                             if (AtomSet.member (!gsymbols, (List.hd a))) then 
                                                                        
                                                                        (
                                                                           let 
                                                                              val t1 = add_same_rules (List.hd a, iset)
                                                                           in
                                                                              ()
                                                                           end 
                                                                        )
                                                                        
                                                                        else ()
                                                            )

                                                         else ()

                                                         end
                     in
                           (*map on all set elements*)
                           map call_add_rule (ItemSet.listItems (!iset))
                     end





type set_type = ItemSet.set
fun print_item_set iset = map print_item (ItemSet.listItems (!iset));
fun print_item_set2 iset = map print_item (ItemSet.listItems (iset));

(*Do above until set changes*)

fun find_closure (iset) = let
                              val closure_old = (!iset)
                              val tt = closure (iset)
                        in 
                          if( ItemSet.equal(closure_old, (!iset)) )
                             
                             then ()
                             else (find_closure (iset))
                       end


fun goto (s, iset, new_set) = let

                           (*input is item set element i.e. an item*)
                           fun add_goto_element (r:item) =  let

                                                               val l = #lhs r
                                                               val b = #before r
                                                               val a = #after r

                                                            in

                                                               (*check size also as goto/shift on last element is a reduce*)
                                                               if (List.null (a) = false andalso ((List.length a) > 1)) then(

                                                                   if (Atom.same(List.hd (a), s))

                                                                   then(
                                                                            let
                                                                                  val goto_element = {lhs = l, before = [s]@b, after = List.tl (a)}
                                                                            in

                                                                                  (*(new_set := ItemSet.add((!new_set), goto_element))*)
                                                                                  (new_set := ItemSet.union(!new_set, ItemSet.singleton goto_element))
                                                                            end
                                                                   )
                                                                   else ()

                                                                   )

                                                               else ()
                                                            end
                     in
                           map add_goto_element (ItemSet.listItems (iset))
                     end
                           

(*should return closure of set got from goto*)
fun get_goto (s, iset, new_set) =   let
                              (*val new_set:set_type ref = ref ItemSet.empty*)
                              val tt = goto (s, iset, new_set) 
                           in
                              find_closure(new_set)
                           end

structure ITEMSET_MAP_KEY:ORD_KEY = struct

         type ord_key = int

         fun compare (i1, i2) = if (i1 = i2) then (EQUAL)

                                 else(
                                          if( i1 > i2) then (GREATER)

                                          else (LESS)

                                 )
end

structure T_MAP_KEY: ORD_KEY = struct
         
         type ord_key = ItemSet.set
         fun compare (iset1, iset2) = ItemSet.compare (iset1, iset2)
end

structure ItemMap = RedBlackMapFn (ITEMSET_MAP_KEY)
structure TMap = RedBlackMapFn (T_MAP_KEY)

type map_type = (ItemSet.set ref) ItemMap.map
type tmap_type = int TMap.map

(*Map for ref set of Items*)
val T:map_type ref = ref ItemMap.empty
val Tnew:tmap_type ref = ref TMap.empty

val state_num = ref 1

(*Initialize T*)
val first_set:set_type ref = ref ItemSet.empty
val z = add_same_rules((Atom.atom "S'"), first_set)
val z = find_closure (first_set);

(*Now add this set to the map*)
T := ItemMap.insert(!T, !state_num, first_set);

(*Define an edge map*)
(*map from state number to list of tuples of symbols and state numbers*)
type symbol_state_pair= (Atom.atom)*(int);
type edge_map_type = (symbol_state_pair list) ItemMap.map

val E:edge_map_type ref = ref ItemMap.empty;
(*add corresponding entry in E map*)
E := ItemMap.insert(!E, !state_num, []);

Tnew := TMap.insert(!Tnew, !first_set, !state_num);

val tmp_set:set_type ref = ref ItemSet.empty;
val tt = get_goto((Atom.atom "S"), !first_set, tmp_set);
val tt = (print("**********goto on S**********");print_item_set(tmp_set));

(*now print T map*)
fun print_T () = let 
                  fun print_element (num, iset) = ( print("state num = ");
                                                    print(Int.toString(num));
                                                    print("\n");
                                                    print_item_set(iset)
                                                  )
               in
                  map print_element (ItemMap.listItemsi (!T))
               end

fun print_Tnew () = let 
                     fun print_element (iset, num) = ( print("state num = ");
                                                       print(Int.toString(num));
                                                       print("\n");
                                                       print_item_set2(iset)
                                                      )
                     in
                        map print_element (TMap.listItemsi (!Tnew))
                     end

val tt = (print("**********TNew************\n"); print_Tnew());

fun print_E () =  let
                     fun print_E_rhs (num, l) = let
                                                            val tt = (print("state num = "); print(Int.toString(num)); print("\n"));

                                                            fun print_E_element (symbol, state2) = (print(Atom.toString(symbol)); 
                                                                                                    print(" state2 = ");
                                                                                                    print(Int.toString(state2));
                                                                                                    print("\n"))
                                                        in
                                                            map print_E_element l
                                                        end
                  in
                        map print_E_rhs (ItemMap.listItemsi(!E))
                  end

val flag:bool ref = ref true;

fun compute_states2 () = let
                              val tt = (flag := false);
                              fun get_goto_set (iset, old_state_num) =  let

                                                            fun add_in_E (symbol, state2) = let
                                                                                    val old_list = ItemMap.lookup(!E, old_state_num)
                                                                              in
                                                                                 ( (flag := true) ;E := ItemMap.insert(!E, old_state_num, old_list@[(symbol, state2)]))
                                                                              end

                                                            fun add_in_Tnew (new_set, symbol) = if (TMap.inDomain(!Tnew, !new_set)) then ()
                                                                                                       
                                                                                                       else (
                                                                                                         state_num := (!state_num) + 1; 
                                                                                                         E := ItemMap.insert(!E, !state_num, []);
                                                                                                         Tnew := TMap.insert(!Tnew, !new_set, !state_num);
                                                                                                         add_in_E(symbol, !state_num)
                                                                                                      )

                                                            (*input is an item*)
                                                            fun call_get_goto (r:item) = let
                                                                                            (*define a new set*)
                                                                                            val new_set:set_type ref = ref ItemSet.empty                                                                                            (*E:= ItemMap.insert(!E, !state_num, []);
                                                                                            add_in_E(aa, !state_num)*)

                                                                                            val l = #lhs r
                                                                                            val b = #before r
                                                                                            val a = #after r

                                                                                    in
                                                                                          (*if (!Atom.same(aa, (Atom.atom "$"))) then*)
                                                                                          if (List.null (a) = false andalso ((List.length a) > 1)) then

                                                                                          ( get_goto(List.hd a, iset, new_set); 
                                                                                            add_in_Tnew(new_set, List.hd a)
                                                                                          )
                                                                                          
                                                                                          else ()
                                                                                    end

                                                         in

                                                            (*map over elements of iset*)
                                                            map call_get_goto (ItemSet.listItems (iset))

                                                         end

                        in
                           (*map on all item sets in t*)
                           map  get_goto_set (TMap.listItemsi (!Tnew))

                        end

val count = ref 1;
fun compute_lr_states () = if (!count < 10) then (

                                 let 
                                     val tt = (count := (!count) + 1)
                                 in
                                     (print("hi");compute_states2 (); compute_lr_states ())
                                 end
                           )

                           else ()


val tt = compute_lr_states ();
val tt = (print("**********TNew************\n"); print_Tnew());
val tt = (print("**********E************\n"); print_E());

fun get_reduce () = let

                        (*input is an item set*)
                        fun output_reduce (iset, state_number) = let

                                                      fun can_reduce (r:item) = let

                                                                                 val l = #lhs r
                                                                                 val b = #before r
                                                                                 val a = #after r

                                                                                in
                                                                                    if(List.length a = 1) then (

                                                                                       let 
                                                                                          val reduced_state = {lhs = l, before = a@b, after = []};
                                                                                       in
                                                                                          (print("For state "); print(Int.toString(state_number));
                                                                                           print(" reduce on "); print(Atom.toString (List.hd (a)));print(" to\n");
                                                                                           print_item(reduced_state);print("\n")
                                                                                       )
                                                                                       end
                                                                                    )

                                                                                    else ()

                                                                                end     
                                                   in
                                                      map can_reduce (ItemSet.listItems (iset))
                                                   end


                     in
                        map output_reduce (TMap.listItemsi (!Tnew))

                     end

val tt = print("*************Reduce*************\n");
val tt = get_reduce ();              

val tt = print("*************For SLR************\n");
(*input is lhs symbol and a singleton list of Atom*)
fun print_follow_symbols (l, a) = let
                                 val symbol = List.hd a;                         (*Atom element*)
                                 val follow_set = AtomMap.lookup(!follow, l);    (*Atom set of follow symbols*)
                              in
                                 (*AtomSet.member (follow_set, symbol)*)
                                 (print_set (follow_set))
                              end

fun get_reduce_slr () = let

                        (*input is an item set*)
                        fun output_reduce (iset, state_number) = let

                                                      fun can_reduce (r:item) = let

                                                                                 val l = #lhs r
                                                                                 val b = #before r
                                                                                 val a = #after r

                                                                                in
                                                                                    if(List.length a = 1) then (

                                                                                          let 
                                                                                             val reduced_state = {lhs = l, before = a@b, after = []};
                                                                                          in
                                                                                             (print("For state "); print(Int.toString(state_number));
                                                                                              print(" reduce on "); print(Atom.toString (List.hd (a)));print(" to\n");
                                                                                              print_item(reduced_state);print("\n");
                                                                                              print("On symbols\n");
                                                                                              print_follow_symbols(l, a);
                                                                                              print("\n")
                                                                                          )
                                                                                          end
                                                                                    )

                                                                                    else ()

                                                                                end     
                                                   in
                                                      map can_reduce (ItemSet.listItems (iset))
                                                   end


                     in
                        map output_reduce (TMap.listItemsi (!Tnew))

                     end

val tt = get_reduce_slr ();
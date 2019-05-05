(*RHS of production is represented as list of Atom
* Y -> Aa
* RHS = [A, a]
*)

type RHS = Atom.atom list

(*Implement compare function for set*)
structure RHS_KEY: ORD_KEY = struct

        type ord_key = RHS

        fun compare ([], [])         = EQUAL
          | compare (x , [])         = GREATER
          | compare ([], y )         = LESS
          | compare (x::xs, y::ys) = case Atom.same(x,y) of
                                       true => compare(xs, ys)
                                     | false => Atom.compare (x, y);
end

(* For any symbol 
*  its RHS productions are captured
*  in set of RHS i.e. Set of (List of Atoms) 
* *)
structure RHSSet = RedBlackSetFn (RHS_KEY)

(*Initialize type Productions*)
type Productions = RHSSet.set

(*All grammar rules are a Map of productions*)
(*type Rules = Productions AtomMap.map*)

type Rules  =  Productions AtomMap.map

type Grammar = {symbols: AtomSet.set, tokens: AtomSet.set, rules: Rules}


val gsymbols         =  ref AtomSet.empty;
val gtokens          =  ref AtomSet.empty;
val grules:Rules ref =  ref AtomMap.empty;

fun add_symbol x = (gsymbols := AtomSet.add (!gsymbols, Atom.atom x))
fun add_tokens x = (gtokens  := AtomSet.add (!gtokens , Atom.atom x))

fun print_Atom a = (print (Atom.toString a); print(" "))

fun list_map (f ,(x::xs)) = [f x]@(list_map (f, xs))
  | list_map (f , [])     = []

fun print_set aset = list_map (print_Atom, AtomSet.listItems aset)

fun get_RHS (r::rs) = [Atom.atom r]@(get_RHS rs)
  | get_RHS []    = []


fun add_rule (s, r) = let val right = list_map (get_RHS, r) in
                                (grules := AtomMap.insert(!grules, Atom.atom s, RHSSet.fromList right))
                           end

fun print_atom_list al = (list_map (print_Atom, al);print("\n"))

(* function to print productions*)
fun print_productions (s, p) = (print_Atom s;
                                        print(" "); list_map (print_atom_list, RHSSet.listItems p))

fun print_productions (s, p) = let 
                                  val l = RHSSet.listItems p

                                  fun x lnew = (print_Atom s; print ("--> "); print_atom_list lnew);
                               in
                                 list_map (x, l)
                                end 

fun print_map m = list_map (print_productions, AtomMap.listItemsi m)


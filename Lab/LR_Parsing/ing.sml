(*add_symbol ("S'");
add_symbol ("S");
add_symbol ("L");

add_tokens "$";
add_tokens "(";
add_tokens ")";
add_tokens ",";
add_tokens  "x";

add_rule("S'",[["S","$"]]);
add_rule("S",[["(", "L", ")"], ["x"]]);
add_rule("L",[["S"], ["L",",","S"]]);
*)
add_symbol ("S'");
add_symbol ("S");
add_symbol ("B");

add_tokens "$";
add_tokens "c";

add_rule("S'",[["S","$"]]);
add_rule("S",[["B"]]);
add_rule("B",[["c"]]);
print_map (!grules);

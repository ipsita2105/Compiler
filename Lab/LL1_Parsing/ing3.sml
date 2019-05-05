add_symbol ("S");
add_symbol ("A");
add_symbol ("B");
add_symbol ("C");

add_tokens "d";

add_rule("S",[["A"]]);
add_rule("A",[["B"]]);
add_rule("B",[["C"]]);
add_rule("C",[["d"]]);

print_map (!grules);

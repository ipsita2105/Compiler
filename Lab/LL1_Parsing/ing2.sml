add_symbol ("S");
add_symbol ("A");
add_symbol ("B");

add_tokens "a";
add_tokens "b";

add_rule("S",[["A","a"],["B"]]);
add_rule("A",[["a"]]);
add_rule("B",[["b"]]);

print_map (!grules);

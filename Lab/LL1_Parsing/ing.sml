add_symbol ("S");
add_symbol ("E");
add_symbol ("E'");
add_symbol ("T");
add_symbol ("T'");

add_tokens "+";
add_tokens "*";
add_tokens "N";
add_tokens "$";

add_rule("S",[["E","$"]]);
add_rule("E",[["T","E'"]]);
add_rule("E'",[["+","T","E'"], []]);
add_rule("T",[["N","T'"]]);
add_rule("T'",[["*","N","T'"], []]);

print_map (!grules);

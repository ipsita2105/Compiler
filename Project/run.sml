val t = Parse.parse "./testcases/factorial.c";
val tt = PrettyPrint.pprint (t);
val tt = print("************************* Generated Code************************\n");
val tt = CodeGen.code_generator(t);

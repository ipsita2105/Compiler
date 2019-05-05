structure Types =
struct

    datatype ty = INT
                | CHAR
                | BOOL
                | VOID
                | INTARRAY
                | CHARARRAY
                | BOOLARRAY
                | FUNCTION of (ty list*ty)

    fun type_to_string (t:ty) = case t of
                                     INT  => ("int ") 
                                  |  CHAR => ("char ")
                                  |  BOOL => ("bool ")
                                  |  VOID => ("void ")
                                  |  CHARARRAY => ("char array ")
                                  |  BOOLARRAY => ("bool array ")
                                  |  INTARRAY  => ("int array ")
                                  | _          => ("other type")
    
    fun funtype_to_string (paralist:ty list, rt:ty) = 

                                           let
                                                fun print_type x = (print(type_to_string x))
                                                       
                                           in
                                                (map print_type paralist);
                                                (print_type rt)
                                           end

end


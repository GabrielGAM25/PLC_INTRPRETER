use "Plc.sml";

exception TestsFailed;

let 
    val _ = teval (fromString "foo") [("foo", BoolT)];
in
    let
        val _ = teval (fromString "foo") [];
    in
        raise TestsFailed
    end handle SymbolNotFound => print ("PASSED => SymbolNotFound were succesfully raised\n")
end handle _ => print("\n\027[1;31mError! Tests failed\027[0m\n\n");

print("\n\027[1;32mSuccess! All tests passed\027[0m\n\n")

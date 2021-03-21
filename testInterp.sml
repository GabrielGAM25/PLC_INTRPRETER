use "Plc.sml";

exception TestsFailed;

print("\n\027[1;35mRunning tests...\027[0m\n\n");

let
  val _ = if eval (fromString "0") [] = IntV 0 then true else raise TestsFailed;
  val _ = if eval (fromString "true") [] = BoolV true then true else raise TestsFailed;
  val _ = if eval (fromString "()") [] = ListV [] then true else raise TestsFailed;
  val _ = if eval (fromString "(0,1,2)") [] = ListV [IntV 0, IntV 1, IntV 2] then true else raise TestsFailed;
  val _ = if eval (fromString "(0,1,2)[1]") [] = IntV 0 then true else raise TestsFailed;
  val _ = if eval (fromString "(0,1,2)[2]") [] = IntV 1 then true else raise TestsFailed;
  val _ = if eval (fromString "(0,1,foo)[3]") [("foo", IntV 3)] = IntV 3 then true else raise TestsFailed;
  val _ = if eval (fromString "(0,1,foo)[2]") [("foo", ListV [IntV 3])] = IntV 1 then true else raise TestsFailed;
  val _ = if eval (fromString "(true, 0, false)[1]") [] = BoolV true then true else raise TestsFailed;
  val _ = if eval (fromString "(true, 0, false)[2]") [] = IntV 0 then true else raise TestsFailed;
  val _ = if eval (fromString "([Int] [])") [] = SeqV [] then true else raise TestsFailed;
  val _ = if eval (fromString "print foo; 0") [("foo", BoolV true)] = IntV 0 then true else raise TestsFailed;
  val _ = if eval (fromString "fn (Int foo) => foo end") [] = Clos("", "foo", Var "foo", []) then true else raise TestsFailed;
  val _ = if eval (fromString "fun f(Bool foo) = !foo; f(true)") [] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "fun mult(Int foo) = fn (Int bar) => foo*bar end; mult(12)(3)") [] = IntV 36 then true else raise TestsFailed;
  val _ = if eval (fromString "fun rec f(Int n):Int = if n < 3 then 1 else f(n-1) + f(n-2); f(7)") [] = IntV 13 then true else raise TestsFailed;
  val _ = if eval (fromString "!foo") [("foo", BoolV true)] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "-1") [] = IntV ~1 then true else raise TestsFailed;
  val _ = if eval (fromString "-foo") [("foo", IntV ~1)] = IntV 1 then true else raise TestsFailed;
  val _ = if eval (fromString "hd foo") [("foo", SeqV [IntV 0, IntV 1])] = IntV 0 then true else raise TestsFailed;
  val _ = if eval (fromString "tl foo") [("foo", SeqV [IntV 0, IntV 1, IntV 2])] = SeqV [IntV 1, IntV 2] then true else raise TestsFailed;
  val _ = if eval (fromString "ise foo") [("foo", SeqV [IntV 0, IntV 1])] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "ise foo") [("foo", SeqV [])] = BoolV true then true else raise TestsFailed;
  val _ = if eval (fromString "true && false") [] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "4+2") [] = IntV 6 then true else raise TestsFailed;
  val _ = if eval (fromString "4-2") [] = IntV 2 then true else raise TestsFailed;
  val _ = if eval (fromString "4*2") [] = IntV 8 then true else raise TestsFailed;
  val _ = if eval (fromString "4/2") [] = IntV 2 then true else raise TestsFailed;
  val _ = if eval (fromString "4<2") [] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "4<=2") [] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "2<4") [] = BoolV true then true else raise TestsFailed;
  val _ = if eval (fromString "2<=4") [] = BoolV true then true else raise TestsFailed;
  val _ = if eval (fromString "2=4") [] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "2=2") [] = BoolV true then true else raise TestsFailed;
  val _ = if eval (fromString "2!=4") [] = BoolV true then true else raise TestsFailed;
  val _ = if eval (fromString "2!=2") [] = BoolV false then true else raise TestsFailed;
  val _ = if eval (fromString "print false") [] = ListV [] then true else raise TestsFailed;
  val _ = if eval (fromString "print foo; foo") [("foo", IntV 1)] = IntV 1 then true else raise TestsFailed;
  val _ = if eval (fromString "1::2::([Int] [])") [] = SeqV [IntV 1, IntV 2] then true else raise TestsFailed;
  val _ = if eval (fromString "(0,false)::(1,true)::([(Int,Bool)] [])") [] = SeqV [(ListV [IntV 0, BoolV false]), (ListV [IntV 1, BoolV true])] then true else raise TestsFailed;
  val _ = if eval (fromString "(0,false)::(1,true)::foo") [("foo", SeqV [ListV [IntV 2, BoolV false]])] = SeqV [(ListV [IntV 0, BoolV false]), (ListV [IntV 1, BoolV true]), (ListV [IntV 2, BoolV false])] then true else raise TestsFailed;
  val _ = if eval (fromString "if foo = 4 then 0 else 1") [("foo", IntV 4)] = IntV 0 then true else raise TestsFailed;
  val _ = if eval (fromString "if foo = 4 then 0 else 1") [("foo", IntV 5)] = IntV 1 then true else raise TestsFailed;
  val _ = if eval (fromString "match foo with | false -> 0 | true -> 1 end") [("foo", BoolV false)] = IntV 0 then true else raise TestsFailed;
  val _ = if eval (fromString "match foo with | false -> 0 | true -> 1 end") [("foo", BoolV true)] = IntV 1 then true else raise TestsFailed;
  val _ = if eval (fromString "var foo = 10; var bar = 20; foo + bar") [] = IntV 30 then true else raise TestsFailed;
  val _ = let
      val _ = eval (fromString "0::1") []
    in 
      raise TestsFailed
    end handle Impossible => {};
  val _ = let
      val _ = eval (fromString "hd ([Int] [])") []
    in 
      raise TestsFailed
    end handle HDEmptySeq => {};
  val _ = let
      val _ = eval (fromString "tl ([Int] [])") []
    in 
      raise TestsFailed
    end handle TLEmptySeq => {};
  val _ = let
      val _ = eval (fromString "foo(1)") [("foo", IntV 0)]
    in 
      raise TestsFailed
    end handle NotAFunc => {};
  val _ = let
      val _ = eval (fromString "match foo with | 0 -> 1 end") [("foo", IntV 1)]
    in 
      raise TestsFailed
    end handle ValueNotFoundInMatch => {};
in
  print("\n\027[1;32mSuccess! All tests passed\027[0m\n\n")
end handle _ => print("\n\027[1;31mError! Tests failed\027[0m\n\n");

use "Plc.sml";

exception TestsFailed;

print("\n\027[1;35mRunning tests...\027[0m\n\n");

let
  val _ = if teval (fromString "0") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "true") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "()") [] = ListT [] then true else raise TestsFailed;
  val _ = if teval (fromString "(0,1,2)") [] = ListT [IntT, IntT, IntT] then true else raise TestsFailed;
  val _ = if teval (fromString "(0,1,2)[1]") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "(0,1,2)[2]") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "(0,1,foo)[3]") [("foo", IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "(0,1,foo)[2]") [("foo", SeqT IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "(true, 0, false)[1]") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "(true, 0, false)[2]") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "([Int] [])") [] = SeqT IntT then true else raise TestsFailed;
  val _ = if teval (fromString "print foo; 0") [("foo", BoolT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "fn (Int foo) => foo end") [] = FunT (IntT, IntT) then true else raise TestsFailed;
  val _ = if teval (fromString "var foo = true; !foo") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "fun f(Bool foo) = !foo; f(true)") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "fun mult(Int foo) = fn (Int bar) => foo*bar end; mult(12)(3)") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "fun rec f(Int n):Int = if n < 3 then 1 else f(n-1) + f(n-2); f(7)") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "-1") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "-foo") [("foo", IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "hd foo") [("foo", SeqT IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "tl foo") [("foo", SeqT IntT)] = SeqT IntT then true else raise TestsFailed;
  val _ = if teval (fromString "ise foo") [("foo", SeqT IntT)] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "true && false") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "4+2") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "4-2") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "4*2") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "4/2") [] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "4<2") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "4<=2") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "2<4") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "2<=4") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "2=4") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "2=2") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "2!=4") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "2!=2") [] = BoolT then true else raise TestsFailed;
  val _ = if teval (fromString "print false") [] = ListT [] then true else raise TestsFailed;
  val _ = if teval (fromString "print foo; foo") [("foo", IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "1::2::([Int] [])") [] = SeqT IntT then true else raise TestsFailed;
  val _ = if teval (fromString "(0,false)::(1,true)::([(Int,Bool)] [])") [] = SeqT (ListT [IntT, BoolT]) then true else raise TestsFailed;
  val _ = if teval (fromString "(0,false)::(1,true)::foo") [("foo", SeqT (ListT [IntT, BoolT]))] = SeqT (ListT [IntT, BoolT]) then true else raise TestsFailed;
  val _ = if teval (fromString "if foo = 4 then 0 else 1") [("foo", IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "match foo with | false -> 0 | true -> 1 end") [("foo", BoolT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "var foo = 0; if foo < 0 then foo else 1") [("foo", IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "var foo = 0; if foo < 1 then 1 else foo") [("foo", IntT)] = IntT then true else raise TestsFailed;
  val _ = if teval (fromString "var foo = 10; var bar = 20; foo + bar") [] = IntT then true else raise TestsFailed;
  val _ = let
      val _ = teval (fromString "0::1") [];
    in
      raise TestsFailed
    end handle UnknownType => {};

  val _ = let
      val _ = teval (fromString "(Int [])") [];
    in
      raise TestsFailed
    end handle EmptySeq => {};

  val _ = let
      val _ = teval (fromString "match foo with | false -> 0 | true -> false end") [("foo", BoolT)];
    in
      raise TestsFailed
    end handle MatchResTypeDiff => {};

  val _ = let
      val _ = teval (fromString "match foo with | false -> 0| true -> 1 end") [("foo", IntT)];
    in
      raise TestsFailed
    end handle MatchCondTypesDiff => {};

  val _ = let
      val _ = teval (fromString "if foo then 0 else 1") [("foo", IntT)];
    in
      raise TestsFailed
    end handle IfCondNotBool => {};

  val _ = let
      val _ = teval (fromString "if foo then 0 else false") [("foo", BoolT)];
    in
      raise TestsFailed
    end handle DiffBrTypes => {};

  val _ = let
      val _ = teval (fromString "(1,2,3)[4]") [];
    in
      raise TestsFailed
    end handle ListOutOfRange => {};

  val _ = let
      val _ = teval (fromString "true = 1") [];
    in
      raise TestsFailed
    end handle NotEqTypes => {};

  val _ = let
      val _ = teval (fromString "fun rec f():Bool = 0; f()") [];
    in
      raise TestsFailed
    end handle WrongRetType => {};

  val _ = let
      val _ = teval (fromString "fun rec f():Bool = 0; f(1)") [];
    in
      raise TestsFailed
    end handle CallTypeMisM => {};

  val _ = let
      val _ = teval (fromString "foo(1)") [("foo", IntT)];
    in
      raise TestsFailed
    end handle NotFunc => {};

  val _ = let
      val _ = teval (fromString "foo[1]") [("foo", IntT)];
    in
      raise TestsFailed
    end handle OpNonList => {};
in
  print("\n\027[1;32mSuccess! All tests passed\027[0m\n\n")
end handle _ => print("\n\027[1;31mError! Tests failed\027[0m\n\n");

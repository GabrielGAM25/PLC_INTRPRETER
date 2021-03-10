%%

%name PlcParser

%pos int

%term VAR
    | FUN | REC | COLON
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT
    | HD | TL | ISE
    | PRINT
    | AND
    | PLUS | MINUS | TIMES | DIV | EQUAL | DIFF | LT | LTE
    | DCOL
    | SEMICOL
    | LSBR | RSBR | LPAR | RPAR | LBR | RBR
    | ANONF | AFARROW | END
    | TRUE | FALSE
    | COMMA
    | PIPE | MARROW
    | UNDERSCORE
    | NIL | BOOL | INT
    | CINT of int | CBOOL of bool | CNIL of nil | NAME of string
    | EOF

%nonterm Prog of expr
  | Decl of expr
  | Expr of expr
  | AtomExpr of expr
  | AppExpr of expr
  | Const of expr
  | Comps of expr
  | MatchExpr of expr
  | CondExpr of expr
  | Args of plcType
  | Params of expr
  | TypedVar of expr
  | Type of expr
  | AtomType of expr
  | Types of expr

%left ELSE AND EQUAL DIFF LT LTE PLUS MINUS TIMES DIV
%right SEMICOL MARROW DCOL
%nonassoc IF NOT HD TL ISE PRINT

%eop EOF

%noshift EOF

%start Prog

%%

Prog: Expr (Expr) 
    | Decl (Decl)

Decl: VAR NAME EQUAL Expr SEMICOL Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQUAL Expr SEMICOL Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN REC NAME Args COLON Type EQUAL Expr SEMICOL Prog (MakeFun(NAME, Args, Type, Expr, Prog))

Expr: AtomExpr(AtomExpr)
    | AppExpr(AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("!", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise"), Expr)
    | PRINT Expr (Prim1("print"), Expr)
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr TIMES Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | MINUS Expr (Prim1("-", Expr))
    | Expr EQUAL Expr (Prim2("=", Expr1, Expr2))
    | Expr DIFF Expr (Prim2("!=", Expr1, Expr2))
    | Expr LT Expr (Prim2("<", Expr1, Expr2))
    | Expr LTE Expr (Prim2("<=", Expr1, Expr2))
    | Expr DCOL Expr (Prim2("::", Expr1, Expr2))
    | Expr LSBR CINT RSBR (Item (CINT, Expr))

AtomExpr: Const (Const)
    | NAME (Var(NAME))
    | LBR Prog RBR (Prog)
    | LPAR Comps RPAR (List Comps)
    | LPAR Expr RPAR (Expr)
    | ANONF Args AFARROW Expr END (makeAnon(Args, Expr))

AppExpr: AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const: TRUE (ConB(TRUE)) | FALSE (ConB(FALSE))
    | CINT (ConI(CINT))
    | LPAR RPAR (Nil)
    | LPAR Type LSBR RSBR RPAR (ESeq(Type))

Comps: Expr COMMA Expr (Expr1::Expr2::[])
    | Expr COMMA Comps (Expr::Comps)

MatchExpr: END (Nil)
    | PIPE CondExpr MARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr: Expr (SOME Expr)
    | UNDERSCORE (NONE)

Args: LPAR RPAR ([])
    | LPAR Params RPAR (Params)
    
Params: TypedVar (TypedVar)
    | TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type NAME ((Type, NAME))

Type: AtomType(AtomType)
    | LPAR Types RPAR (ListT [])
    | LSBR Type RSBR (SeqT Type)
    | Type MARROW Type (FunT (Type1, Type2))

AtomType: NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types: Type COMMA Type (Type1::Type2::[])
    | Type COMMA Types (Type::Types)

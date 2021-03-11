(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type('a,'b) token =('a,'b) Tokens.token
type lexresult =(slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun keyWord(s, lpos, rpos) =
    case s of 
        "var" => VAR(lpos, rpos)
        | "fun" => FUN(lpos, rpos)
        | "rec" => REC(lpos, rpos)
        | "if" => IF(lpos, rpos)
        | "then" => THEN(lpos, rpos)
        | "else" => ELSE(lpos, rpos)
        | "match" => MATCH(lpos, rpos)
        | "with" => WITH(lpos, rpos)
        | "hd" => HD(lpos, rpos)
        | "tl" => TL(lpos, rpos)
        | "ise" => ISE(lpos, rpos)
        | "print" => PRINT(lpos, rpos)
        | "fn" => ANONF(lpos, rpos)
        | "end" => END(lpos, rpos)
        | "true" => TRUE(lpos, rpos)
        | "false" => FALSE(lpos, rpos)
        | "_" => UNDERSCORE(lpos, rpos)
        | "Nil" => NIL(lpos, rpos)
        | "Bool" => BOOL(lpos, rpos)
        | "Int" => INT(lpos, rpos)
        | _   => NAME(s, lpos, rpos)

fun stoi s =
    case Int.fromString s of
        SOME i => i
        | NONE => raise Fail("Não foi possível converter a string '" ^ s ^ "' para inteiro")

(* Define what to do when the end of the file is reached. *)
fun eof() = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() =()

%%
%header(functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%s COMMENT;
startcomment=\(\*;
endcomment=\*\);

%%

\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>{whitespace}+ => (lex());
<INITIAL>{digit}+ => (CINT(stoi(yytext), yypos, yypos));
<INITIAL>{identifier} => (keyWord(yytext, yypos, yypos));
<INITIAL>":" => (COLON(yypos, yypos));
<INITIAL>"!" => (NOT(yypos, yypos));
<INITIAL>"&&" => (AND(yypos, yypos));
<INITIAL>"+" => (PLUS(yypos, yypos));
<INITIAL>"-" => (MINUS(yypos, yypos));
<INITIAL>"*" => (TIMES(yypos, yypos));
<INITIAL>"/" => (DIV(yypos, yypos));
<INITIAL>"=" => (EQUAL(yypos, yypos));
<INITIAL>"!=" => (DIFF(yypos, yypos));
<INITIAL>"<" => (LT(yypos, yypos));
<INITIAL>"<=" => (LTE(yypos, yypos));
<INITIAL>"::" => (DCOL(yypos, yypos));
<INITIAL>";" => (SEMICOL(yypos, yypos));
<INITIAL>"[" => (LSBR(yypos, yypos));
<INITIAL>"]" => (RSBR(yypos, yypos));
<INITIAL>"(" => (LPAR(yypos, yypos));
<INITIAL>")" => (RPAR(yypos, yypos));
<INITIAL>"{" => (LBR(yypos, yypos));
<INITIAL>"}" => (RBR(yypos, yypos));
<INITIAL>"=>" => (AFARROW(yypos, yypos));
<INITIAL>"," => (COMMA(yypos, yypos));
<INITIAL>"|" => (PIPE(yypos, yypos));
<INITIAL>"->" => (MARROW(yypos, yypos));
<INITIAL>{startcomment} => (YYBEGIN COMMENT; lex());
<COMMENT>{endcomment} => (YYBEGIN INITIAL; lex());
<COMMENT>. => (lex());
<INITIAL>. => (error("\n***Lexer error bad character ***\n"); raise Fail("Lexer error: bad character " ^yytext));
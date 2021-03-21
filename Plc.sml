(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run exp =
    let
      val expType = teval exp []
      val expResult = eval exp []
    in
      val2string(expResult) ^ " : " ^ type2string(expType)

      handle SymbolNotFound => "Trying to use an undefined symbol."
      | EmptySeq => "Trying to define empty sequence without a type."
      | UnknownType => "Invalid operand type."
      | NotEqTypes => "Both operands should have the same type."
      | WrongRetType => "Function returning wrong type."
      | DiffBrTypes => "Branches from conditional have different types."
      | IfCondNotBool => "Conditional expression is not a boolean."
      | NoMatchResults => "Empty pattern matching options."
      | MatchResTypeDiff => "Results from match have different types."
      | MatchCondTypesDiff => "Pattern matching options have different types."
      | CallTypeMisM => "Function called with wrong argument types."
      | NotFunc => "Trying to call a non-function variable as a function."
      | ListOutOfRange => "Trying to access invalid index from list."
      | OpNonList => "Trying to use a non-list variable as a list."
      | HDEmptySeq => "Trying to get the head of an empty sequence."
      | TLEmptySeq => "Trying to get the tail of an empty sequence."
      | ValueNotFoundInMatch => "Expression didn't match any of the match expressions."
      | NotAFunc => "Trying to call a non-function variable as a function."
      | Impossible => "Cannot perform operation."
      | _ => "An unexpected exception was raised."
    end
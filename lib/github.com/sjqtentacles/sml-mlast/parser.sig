(* parser.sig

   Recursive-descent parser for the SML'97 (Core + Modules) subset modelled by
   Ast. Infix expressions/patterns are resolved with a fixity environment that
   starts at the Basis defaults and is updated by infix/infixr/nonfix
   declarations as they are encountered (so user fixity affects parsing).

   Grouping parentheses are not retained in the tree. Raises Parse on a syntax
   error. Pure and deterministic. *)

signature PARSER =
sig
  exception Parse of string
  val parse       : Token.token list -> Ast.program
  val parseString : string -> Ast.program
  val parseExp    : string -> Ast.exp
end

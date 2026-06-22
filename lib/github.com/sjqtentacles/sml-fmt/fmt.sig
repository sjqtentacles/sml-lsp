(* fmt.sig

   A deterministic source formatter built on the sml-mlast frontend: parse the
   text to an Ast and re-render it with sml-mlast's fixed layout rules (full
   parenthesisation of infix/typed/andalso/orelse, 2-space block indentation,
   one declaration per line), terminated by a single newline.

   The defining property is idempotence: `string (string s) = string s`.
   Raises Parser.Parse / Lexer.Lex on malformed input. *)

signature FORMAT =
sig
  (* Format a source string. *)
  val string : string -> string
  (* Read a file and return its formatted contents. *)
  val file   : string -> string
end

(* lexer.sig

   Hand-written scanner from a source string to a Token list (terminated by
   Token.EOF). Handles nested (* ... *) comments, qualified identifiers
   (A.B.c as a single ID), symbolic identifiers, tyvars, integer/word/real
   literals (spelling preserved verbatim) and string/char literals (decoded to
   their payload). Raises Lex on malformed input. Pure and deterministic. *)

signature LEXER =
sig
  exception Lex of string
  val tokenize : string -> Token.token list
end

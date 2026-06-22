(* src/json.sig -- public signature for the sml-json AST + parser.

   `json` is the JSON value tree (RFC 8259). Numbers are split at parse time
   into `JInt` (no '.', 'e', or 'E') and `JReal` (has a fraction/exponent);
   objects keep their members in source order and preserve duplicate keys.

   `parseJson` parses a whole document and rejects trailing garbage and empty
   input. It returns the vendored sml-parsec `result`: `Ok v` on success or
   `Err e` carrying the position/expected-set (`CharParsec.errorToString` renders
   a human-readable message). Serializers live in `structure JsonPretty`
   (`toString`, `toStringIndent`). *)

signature JSON =
sig
  datatype json =
      JNull
    | JBool of bool
    | JInt  of int     (* JSON number with no '.', 'e', or 'E' *)
    | JReal of real    (* JSON number with a fraction/exponent *)
    | JStr  of string
    | JArr  of json list
    | JObj  of (string * json) list

  (* Parse a complete JSON document. Leading/trailing whitespace is allowed;
     trailing non-whitespace and empty/whitespace-only input are errors. *)
  val parseJson : string -> json CharParsec.result
end

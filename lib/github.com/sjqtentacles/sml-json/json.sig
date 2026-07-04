(* src/json.sig -- public signature for the sml-json AST + parser.

   `json` is the JSON value tree (RFC 8259). Numbers are split at parse time
   into `JInt` (no '.', 'e', or 'E') and `JReal` (has a fraction/exponent);
   objects keep their members in source order and preserve duplicate keys.

   `JInt` carries an arbitrary-precision `IntInf.int`, so integers of any size
   (large ids, millisecond timestamps, …) parse losslessly and *identically*
   under MLton and Poly/ML, whose default `int` types are fixed-width (32-bit
   and 63-bit here) -- a naive `int` would overflow under MLton on values past
   ~2^31. Use `asInt` to narrow to a machine `int` where you know the value
   fits the portable 32-bit range.

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
    | JInt  of IntInf.int  (* JSON number with no '.', 'e', or 'E' (any size) *)
    | JReal of real        (* JSON number with a fraction/exponent *)
    | JStr  of string
    | JArr  of json list
    | JObj  of (string * json) list

  (* Parse a complete JSON document. Leading/trailing whitespace is allowed;
     trailing non-whitespace and empty/whitespace-only input are errors. *)
  val parseJson : string -> json CharParsec.result

  (* Narrow a `JInt` to a machine `int`; `NONE` for a non-integer or a value
     outside the portable signed 32-bit range (identical on both compilers).
     A safe replacement for pattern-matching `JInt n` and using `n` as an `int`. *)
  val asInt : json -> int option
end

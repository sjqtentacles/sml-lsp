(* src/jsonPretty.sml -- JSON serializers (minified + indented).
 *
 * Two entry points:
 *   toString       : json -> string        compact, no insignificant whitespace
 *   toStringIndent : int -> json -> string  pretty, `n`-space indent per level
 *
 * Both re-escape strings to valid JSON and print numbers in JSON form (SML's
 * `~` sign is translated to `-`). Output of either is guaranteed to re-parse to
 * a jsonEq-equal value (see the round-trip tests). *)

structure JsonPretty =
struct
  open Json

  (* SML prints negatives as `~3`; JSON requires `-3`. Translate a leading `~`.
   * (`~` only ever appears at the front for Int/Real.toString output.) *)
  fun fixSign s =
      if String.size s > 0 andalso String.sub (s, 0) = #"~"
      then "-" ^ String.extract (s, 1, NONE)
      else s

  (* Escape one character for inclusion in a JSON string literal. *)
  fun escapeChar c =
      case c of
          #"\"" => "\\\""
        | #"\\" => "\\\\"
        | #"\b" => "\\b"
        | #"\f" => "\\f"
        | #"\n" => "\\n"
        | #"\r" => "\\r"
        | #"\t" => "\\t"
        | _ =>
            if Char.isCntrl c
            then (* other control chars -> \uXXXX (BMP, low byte) *)
              let
                val hex = Int.fmt StringCvt.HEX (ord c)
                val padded = StringCvt.padLeft #"0" 4 hex
              in
                "\\u" ^ String.map Char.toLower padded
              end
            else String.str c

  fun escapeString s = String.concat (List.map escapeChar (String.explode s))

  fun quote s = "\"" ^ escapeString s ^ "\""

  fun numStr (JInt n)  = fixSign (IntInf.toString n)
    | numStr (JReal r) = fixSign (Real.toString r)
    | numStr _ = raise Fail "numStr: not a number"

  (* ---- minified ---- *)
  fun toString json =
      case json of
          JNull     => "null"
        | JBool b   => if b then "true" else "false"
        | JInt _    => numStr json
        | JReal _   => numStr json
        | JStr s    => quote s
        | JArr xs   => "[" ^ String.concatWith "," (List.map toString xs) ^ "]"
        | JObj kvs  =>
            "{" ^ String.concatWith ","
                    (List.map (fn (k, v) => quote k ^ ":" ^ toString v) kvs)
            ^ "}"

  (* ---- indented ----
   * Each nesting level adds `n` spaces. Empty arrays/objects stay on one line
   * (`[]` / `{}`); non-empty containers put each element on its own line. *)
  fun toStringIndent n json =
      let
        val pad = CharVector.tabulate (n, fn _ => #" ")
        fun indent depth = String.concat (List.tabulate (depth, fn _ => pad))
        fun go depth json =
            case json of
                JNull     => "null"
              | JBool b   => if b then "true" else "false"
              | JInt _    => numStr json
              | JReal _   => numStr json
              | JStr s    => quote s
              | JArr []   => "[]"
              | JObj []   => "{}"
              | JArr xs   =>
                  let
                    val inner = indent (depth + 1)
                    val items =
                        List.map (fn v => inner ^ go (depth + 1) v) xs
                  in
                    "[\n" ^ String.concatWith ",\n" items
                    ^ "\n" ^ indent depth ^ "]"
                  end
              | JObj kvs  =>
                  let
                    val inner = indent (depth + 1)
                    val items =
                        List.map (fn (k, v) =>
                                     inner ^ quote k ^ ": " ^ go (depth + 1) v)
                                 kvs
                  in
                    "{\n" ^ String.concatWith ",\n" items
                    ^ "\n" ^ indent depth ^ "}"
                  end
      in
        go 0 json
      end
end

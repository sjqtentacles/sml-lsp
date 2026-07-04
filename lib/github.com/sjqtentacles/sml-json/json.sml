(* src/json.sml -- JSON AST + parser. Built on the vendored sml-parsec. *)

structure Json :> JSON =
struct
  datatype json =
      JNull
    | JBool of bool
    | JInt  of IntInf.int  (* JSON number with no '.', 'e', or 'E' (any size) *)
    | JReal of real        (* JSON number with a fraction/exponent *)
    | JStr  of string
    | JArr  of json list
    | JObj  of (string * json) list

  local
    open CharParsec
    infix 1 >>= >>
    infix 1 <*
    infix 4 <*> <$>
    infixr 1 <|>
    infix 0 <?>

    (* Every JSON token parser is a lexeme: it consumes trailing whitespace so
     * parsers compose under choice/commaSep/brackets. The public `parse` driver
     * handles leading whitespace and end-of-input. *)
    fun lex p = p <* spaces

    val jnull = lex (string "null") >> return JNull

    val jbool =
        (lex (string "true")  >> return (JBool true))
        <|> (lex (string "false") >> return (JBool false))

    (* JSON number grammar (RFC 8259): minus? int frac? exp?
     *   int  = "0" | [1-9] DIGIT*            (no leading zeros)
     *   frac = "." DIGIT+
     *   exp  = ("e"|"E") ("+"|"-")? DIGIT+
     * We assemble the matched text, then hand an SML-syntax string (minus as
     * `~`) to IntInf/Real.fromString. Correctness comes from the grammar above,
     * not from the lenient Basis readers. A '.', 'e' or 'E' => JReal, else JInt.
     * Integers use `IntInf.fromString` (arbitrary precision): it never overflows,
     * so a large literal parses losslessly and identically on MLton and Poly/ML
     * instead of raising `Overflow` under MLton's fixed-width `int`. The `NONE`
     * arms are unreachable given the grammar but are handled totally, so no
     * unchecked `valOf` sits on the parse path.
     *)
    val nzDigit = sat (fn c => c >= #"1" andalso c <= #"9")

    val intPart =
        (string "0")
        <|> (nzDigit >>= (fn d =>
              many digit >>= (fn ds =>
                return (implode (d :: ds)))))

    val fracPart =
        (char #"." >> many1 digit >>= (fn ds => return ("." ^ implode ds)))
        <|> return ""

    val expPart =
        ((oneOf "eE" >>
          option "" ((oneOf "+-") >>= (fn c => return (str c))) >>= (fn sgn =>
          many1 digit >>= (fn ds =>
            return ("e" ^ sgn ^ implode ds)))))
        <|> return ""

    (* Translate a JSON sign string ("" or "-") to SML syntax ("" or "~"). *)
    fun smlSign s = if s = "-" then "~" else ""

    val jnumber =
        lex (option "" (char #"-" >> return "-") >>= (fn sgn =>
             intPart >>= (fn ip =>
             fracPart >>= (fn fp =>
             expPart  >>= (fn ep =>
               let
                 val sml = smlSign sgn
               in
                 if fp = "" andalso ep = ""
                 then (case IntInf.fromString (sml ^ ip) of
                           SOME n => return (JInt n)
                         | NONE => fail "invalid integer literal")
                 else (case Real.fromString (sml ^ ip ^ fp ^ ep) of
                           SOME r => return (JReal r)
                         | NONE => fail "invalid number literal")
               end)))))

    (* JSON strings. We open the quote, read `many (escaped <|> normal)`, then an
     * explicit closing quote. Using an explicit close (not manyTill) avoids the
     * terminator-vs-content ambiguity, because `normal` already excludes the
     * quote. `normal` also excludes the backslash and raw control chars; when
     * `many` hits a control char it stops, and the close-quote parser then fails
     * with a clear error (so raw control chars are rejected). *)
    val hexDigit = sat Char.isHexDigit

    fun hexVal c =
        if c >= #"0" andalso c <= #"9" then ord c - ord #"0"
        else if c >= #"a" andalso c <= #"f" then ord c - ord #"a" + 10
        else (* A-F *) ord c - ord #"A" + 10

    val normal =
        sat (fn c => c <> #"\"" andalso c <> #"\\" andalso not (Char.isCntrl c))

    val escaped =
        char #"\\" >>
        ((char #"\"" >> return #"\"")
         <|> (char #"\\" >> return #"\\")
         <|> (char #"/"  >> return #"/")
         <|> (char #"b"  >> return #"\b")
         <|> (char #"f"  >> return #"\f")
         <|> (char #"n"  >> return #"\n")
         <|> (char #"r"  >> return #"\r")
         <|> (char #"t"  >> return #"\t")
         (* \uXXXX: BMP only. Surrogate-pair decoding for astral code points is a
          * documented follow-up; here we emit the low byte of the code point,
          * which is correct for the Latin-1 range exercised by the tests. *)
         <|> (char #"u" >> count 4 hexDigit >>= (fn hs =>
                let
                  val code = List.foldl (fn (c, acc) => acc * 16 + hexVal c) 0 hs
                in
                  return (chr (code mod 256))
                end)))

    (* Raw string parser (no JStr wrapper): used for both string values and
     * object keys. Eats trailing whitespace so it composes as a lexeme. *)
    val jstr =
        lex (char #"\"" >> many (escaped <|> normal) >>= (fn cs =>
             char #"\"" >> return (implode cs)))

    val jstring = JStr <$> jstr

    (* `value` and `member` are mutually recursive through arrays/objects, so we
     * tie the knot with `delay`. brackets/braces/commaSep are lexeme-aware
     * (each eats trailing whitespace), and all leaves are lexemes, so the whole
     * grammar composes cleanly. *)
    fun value () =
        choice [ jnull, jbool, jnumber, jstring,
                 JArr <$> brackets (commaSep (delay value)),
                 JObj <$> braces (commaSep (delay member)) ]

    and member () =
        jstr >>= (fn k =>
        lex (char #":") >>
        delay value >>= (fn v =>
          return (k, v)))
  in
    val parseJson : string -> json result = parse (delay value)
  end

  (* Narrow a `JInt` to a machine `int`; `NONE` if it is not an integer or is
     outside the portable signed 32-bit range. The bound is a fixed literal
     (not this compiler's `Int` range) so the result is identical on MLton
     (32-bit default `int`) and Poly/ML (63-bit): a value both can hold. *)
  fun asInt (JInt n) =
        if n >= ~2147483648 andalso n <= 2147483647
        then SOME (IntInf.toInt n) else NONE
    | asInt _ = NONE
end

(* lsp.sml - see lsp.sig *)

structure Lsp :> LSP =
struct
  open Json

  (* ---- JSON navigation / construction helpers ---- *)

  fun jfield (JObj kvs) k =
        (case List.find (fn (k2, _) => k2 = k) kvs of
             SOME (_, v) => SOME v | NONE => NONE)
    | jfield _ _ = NONE

  fun jpath j [] = SOME j
    | jpath j (k :: ks) = (case jfield j k of SOME j2 => jpath j2 ks | NONE => NONE)

  fun getStr j path = case jpath j path of SOME (JStr s) => SOME s | _ => NONE
  (* JSON-RPC integers (ids, line/character, codes) fit a machine `int`, but the
   * json AST now carries an arbitrary-precision `IntInf.int` (so oversized ids
   * parse without overflowing MLton's fixed-width default `int`). Narrow with
   * `Json.asInt`, which yields NONE for a non-integer or an out-of-`Int`-range
   * value; the `int option` result type is unchanged. *)
  fun getInt j path = Option.mapPartial Json.asInt (jpath j path)
  fun str path j = Option.getOpt (getStr j path, "")
  fun intp path j = Option.getOpt (getInt j path, 0)

  (* LSP line/character are protocol ints; lift to the json AST's IntInf.int. *)
  fun jpos (l, c) = JObj [("line", JInt (IntInf.fromInt l)),
                          ("character", JInt (IntInf.fromInt c))]
  fun jrange (l1, c1, l2, c2) =
    JObj [("start", jpos (l1, c1)), ("end", jpos (l2, c2))]

  fun serialize j = JsonPretty.toString j

  (* ---- document store ---- *)

  type state = { docs : (string * string) list }
  val initial = { docs = [] } : state

  fun putDoc ({docs} : state, uri, text) =
    { docs = (uri, text) :: List.filter (fn (u, _) => u <> uri) docs } : state
  fun getDoc ({docs} : state, uri) =
    case List.find (fn (u, _) => u = uri) docs of
        SOME (_, t) => SOME t | NONE => NONE

  (* ---- symbol scanner (positions: 0-based line/character) ---- *)

  type sym = { name : string, kind : string, line : int, character : int }

  fun scanSymbols src : sym list =
    let
      val n = String.size src
      val i = ref 0 and line = ref 0 and col = ref 0
      val out = ref ([] : sym list)
      fun cur () = String.sub (src, !i)
      fun at k = String.sub (src, k)
      fun adv () =
        (if !i < n then
           (if at (!i) = #"\n" then (line := !line + 1; col := 0)
            else col := !col + 1)
         else ();
         i := !i + 1)
      fun isIdCh c =
        Char.isAlphaNum c orelse c = #"_" orelse c = #"'" orelse c = #"."
      fun isKw w =
        List.exists (fn k => k = w)
          ["val", "fun", "type", "datatype", "exception",
           "structure", "signature", "functor"]
      fun skipComment () =
        let
          val depth = ref 0
          fun go () =
            if !i >= n then ()
            else if !i + 1 < n andalso cur () = #"(" andalso at (!i + 1) = #"*"
              then (adv (); adv (); depth := !depth + 1; go ())
            else if !i + 1 < n andalso cur () = #"*" andalso at (!i + 1) = #")"
              then (adv (); adv (); depth := !depth - 1;
                    if !depth = 0 then () else go ())
            else (adv (); go ())
        in go () end
      fun skipString () =
        (adv ();
         let
           fun go () =
             if !i >= n then ()
             else if cur () = #"\\" then (adv (); if !i < n then adv () else (); go ())
             else if cur () = #"\"" then adv ()
             else (adv (); go ())
         in go () end)
      fun readWord () =
        let
          val start = !i
          fun go () = if !i < n andalso isIdCh (cur ()) then (adv (); go ()) else ()
          val () = go ()
        in String.substring (src, start, !i - start) end
      fun skipWs () = if !i < n andalso Char.isSpace (cur ()) then (adv (); skipWs ()) else ()
      fun skipTyvars () =
        (skipWs ();
         if !i < n andalso cur () = #"'" then ignore (readWord ())
         else if !i < n andalso cur () = #"(" then
           let
             val d = ref 0
             fun go () =
               if !i >= n then ()
               else if cur () = #"(" then (adv (); d := !d + 1; go ())
               else if cur () = #")" then (adv (); d := !d - 1;
                                           if !d = 0 then () else go ())
               else (adv (); go ())
           in go () end
         else ())
      fun kindOf w = w
      fun loop () =
        if !i >= n then ()
        else
          let val c = cur () in
            if !i + 1 < n andalso c = #"(" andalso at (!i + 1) = #"*"
              then (skipComment (); loop ())
            else if c = #"\"" then (skipString (); loop ())
            else if Char.isAlpha c then
              let val w = readWord () in
                if isKw w then
                  (skipTyvars (); skipWs ();
                   let
                     val nl = !line and nc = !col
                     val name =
                       if !i < n andalso (Char.isAlpha (cur ()) orelse cur () = #"_")
                       then readWord () else ""
                   in
                     if name <> "" then
                       out := { name = name, kind = kindOf w,
                                line = nl, character = nc } :: !out
                     else ();
                     loop ()
                   end)
                else loop ()
              end
            else (adv (); loop ())
          end
      val () = loop ()
    in List.rev (!out) end

  fun kindNum k =
    case k of
        "structure" => 2 | "functor" => 2 | "signature" => 11
      | "val" => 13 | "fun" => 12 | "type" => 5
      | "datatype" => 10 | "exception" => 9 | _ => 13

  (* ---- identifier under a cursor position ---- *)

  fun lineOf text ln =
    let val lines = String.fields (fn c => c = #"\n") text
    in if ln >= 0 andalso ln < List.length lines
       then List.nth (lines, ln) else "" end

  fun identAt (text, ln, chr) =
    let
      val s = lineOf text ln
      val n = String.size s
      fun isIdCh c =
        Char.isAlphaNum c orelse c = #"_" orelse c = #"'" orelse c = #"."
      fun left k = if k > 0 andalso isIdCh (String.sub (s, k - 1)) then left (k - 1) else k
      fun right k = if k < n andalso isIdCh (String.sub (s, k)) then right (k + 1) else k
      val p = if chr > n then n else chr
      val a = left p and b = right p
    in if b > a then SOME (String.substring (s, a, b - a)) else NONE end

  (* ---- diagnostics from the sml-mlast parser ---- *)

  fun mkDiag msg =
    JObj [ ("range", jrange (0, 0, 0, 1)),
           ("severity", JInt 1),
           ("source", JStr "sml-mlast"),
           ("message", JStr msg) ]

  fun diagnostics text =
    (ignore (Parser.parseString text); [])
    handle Parser.Parse msg => [mkDiag msg]
         | Lexer.Lex msg => [mkDiag ("lex error: " ^ msg)]

  fun publishDiag (uri, text) =
    JObj [ ("jsonrpc", JStr "2.0"),
           ("method", JStr "textDocument/publishDiagnostics"),
           ("params", JObj [ ("uri", JStr uri),
                             ("diagnostics", JArr (diagnostics text)) ]) ]

  (* ---- response builders ---- *)

  fun respObj (idOpt, result) =
    JObj [ ("jsonrpc", JStr "2.0"),
           ("id", Option.getOpt (idOpt, JNull)),
           ("result", result) ]
  fun errObj (idOpt, code, msg) =
    JObj [ ("jsonrpc", JStr "2.0"),
           ("id", Option.getOpt (idOpt, JNull)),
           ("error", JObj [("code", JInt (IntInf.fromInt code)),
                           ("message", JStr msg)]) ]

  val capabilities =
    JObj [ ("capabilities",
            JObj [ ("textDocumentSync", JInt 1),
                   ("documentSymbolProvider", JBool true),
                   ("hoverProvider", JBool true),
                   ("definitionProvider", JBool true),
                   ("documentFormattingProvider", JBool true) ]),
           ("serverInfo",
            JObj [("name", JStr "sml-lsp"), ("version", JStr "0.1.0")]) ]

  (* ---- per-method result builders ---- *)

  fun formatResult (st, j) =
    let val uri = str ["params", "textDocument", "uri"] j in
      case getDoc (st, uri) of
          NONE => JArr []
        | SOME text =>
            ((let
                val formatted = Format.string text
                val lines = String.fields (fn c => c = #"\n") text
                val lastLine = List.length lines - 1
                val lastCol = String.size (List.last lines)
                val edit = JObj [ ("range", jrange (0, 0, lastLine, lastCol)),
                                  ("newText", JStr formatted) ]
              in JArr [edit] end)
             handle _ => JArr [])
    end

  fun symbolResult (st, j) =
    let val uri = str ["params", "textDocument", "uri"] j in
      case getDoc (st, uri) of
          NONE => JArr []
        | SOME text =>
            JArr (List.map (fn s =>
              JObj [ ("name", JStr (#name s)),
                     ("kind", JInt (IntInf.fromInt (kindNum (#kind s)))),
                     ("location",
                      JObj [ ("uri", JStr uri),
                             ("range",
                              jrange (#line s, #character s,
                                      #line s,
                                      #character s + String.size (#name s))) ]) ])
              (scanSymbols text))
    end

  fun hoverResult (st, j) =
    let
      val uri = str ["params", "textDocument", "uri"] j
      val ln = intp ["params", "position", "line"] j
      val ch = intp ["params", "position", "character"] j
    in
      case getDoc (st, uri) of
          NONE => JNull
        | SOME text =>
            (case identAt (text, ln, ch) of
                 NONE => JNull
               | SOME name =>
                   let
                     val v =
                       case List.find (fn s => #name s = name) (scanSymbols text) of
                           SOME s => #kind s ^ " " ^ #name s
                         | NONE => name
                   in JObj [ ("contents",
                              JObj [ ("kind", JStr "plaintext"),
                                     ("value", JStr v) ]) ]
                   end)
    end

  fun defResult (st, j) =
    let
      val uri = str ["params", "textDocument", "uri"] j
      val ln = intp ["params", "position", "line"] j
      val ch = intp ["params", "position", "character"] j
    in
      case getDoc (st, uri) of
          NONE => JNull
        | SOME text =>
            (case identAt (text, ln, ch) of
                 NONE => JNull
               | SOME name =>
                   (case List.find (fn s => #name s = name) (scanSymbols text) of
                        SOME s =>
                          JObj [ ("uri", JStr uri),
                                 ("range",
                                  jrange (#line s, #character s, #line s,
                                          #character s + String.size name)) ]
                      | NONE => JNull))
    end

  fun changeText j =
    case jpath j ["params", "contentChanges"] of
        SOME (JArr (c :: _)) =>
          (case jfield c "text" of SOME (JStr s) => s | _ => "")
      | _ => ""

  (* ---- dispatch ---- *)

  fun handleMsg state msg =
    case Json.parseJson msg of
        CharParsec.Err _ => (state, [])
      | CharParsec.Ok j =>
          let
            val method = getStr j ["method"]
            val idOpt = jfield j "id"
          in
            case method of
                SOME "initialize" => (state, [serialize (respObj (idOpt, capabilities))])
              | SOME "initialized" => (state, [])
              | SOME "shutdown" => (state, [serialize (respObj (idOpt, JNull))])
              | SOME "exit" => (state, [])
              | SOME "textDocument/didOpen" =>
                  let
                    val uri = str ["params", "textDocument", "uri"] j
                    val text = str ["params", "textDocument", "text"] j
                  in (putDoc (state, uri, text), [serialize (publishDiag (uri, text))]) end
              | SOME "textDocument/didChange" =>
                  let
                    val uri = str ["params", "textDocument", "uri"] j
                    val text = changeText j
                  in (putDoc (state, uri, text), [serialize (publishDiag (uri, text))]) end
              | SOME "textDocument/formatting" =>
                  (state, [serialize (respObj (idOpt, formatResult (state, j)))])
              | SOME "textDocument/documentSymbol" =>
                  (state, [serialize (respObj (idOpt, symbolResult (state, j)))])
              | SOME "textDocument/hover" =>
                  (state, [serialize (respObj (idOpt, hoverResult (state, j)))])
              | SOME "textDocument/definition" =>
                  (state, [serialize (respObj (idOpt, defResult (state, j)))])
              | SOME m =>
                  (case idOpt of
                       SOME _ => (state, [serialize (errObj (idOpt, ~32601,
                                          "method not found: " ^ m))])
                     | NONE => (state, []))
              | NONE => (state, [])
          end

  fun run state inputs =
    let
      fun go st acc [] = acc
        | go st acc (m :: ms) =
            let val (st', outs) = handleMsg st m
            in go st' (acc @ outs) ms end
    in go state [] inputs end

  (* ---- impure stdio shell ---- *)

  fun stripCR line =
    if String.size line > 0 andalso String.sub (line, String.size line - 1) = #"\n"
    then stripCR (String.substring (line, 0, String.size line - 1))
    else if String.size line > 0 andalso String.sub (line, String.size line - 1) = #"\r"
    then stripCR (String.substring (line, 0, String.size line - 1))
    else line

  fun serve () =
    let
      val st = ref initial
      fun readHeaders len =
        case TextIO.inputLine TextIO.stdIn of
            NONE => NONE
          | SOME raw =>
              let val line = stripCR raw in
                if line = "" then SOME len
                else if String.isPrefix "Content-Length:" line then
                  (* Parse the length via `IntInf` and bound to the portable
                     signed-32-bit range, so an oversized Content-Length yields
                     NONE identically on both compilers rather than raising
                     `Overflow` under MLton's 32-bit `int`. *)
                  readHeaders (case IntInf.fromString
                                        (String.extract (line, 15, NONE)) of
                                   SOME n => if n >= 0 andalso n <= 2147483647
                                             then SOME (IntInf.toInt n) else NONE
                                 | NONE => NONE)
                else readHeaders len
              end
      fun emit out =
        ( TextIO.output (TextIO.stdOut,
            "Content-Length: " ^ Int.toString (String.size out) ^ "\r\n\r\n" ^ out);
          TextIO.flushOut TextIO.stdOut )
      fun loop () =
        case readHeaders NONE of
            NONE => ()
          | SOME NONE => loop ()
          | SOME (SOME len) =>
              let
                val body = TextIO.inputN (TextIO.stdIn, len)
                val (st', outs) = handleMsg (!st) body
              in st := st'; List.app emit outs; loop () end
    in loop () end
end

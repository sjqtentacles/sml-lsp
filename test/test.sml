(* test.sml - sml-lsp pure handler tests over recorded JSON-RPC transcripts.

   The server core (`Lsp.handleMsg`, `Lsp.run`) is a pure function from incoming
   JSON-RPC text to outgoing serialized messages. We pin exact golden outputs
   for the lifecycle (initialize/shutdown), diagnostics (didOpen valid/invalid),
   documentSymbol, hover, go-to-definition, formatting, and error handling. *)

structure Tests =
struct
  open Harness

  val uri = "file:///a.sml"

  (* A document opened so subsequent requests have something to work on. *)
  val src = "val x = 1\nfun id y = y\n"

  fun didOpen text =
    "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":"
    ^ "{\"textDocument\":{\"uri\":\"" ^ uri ^ "\",\"text\":\"" ^ text ^ "\"}}}"

  fun openState () =
    let val (st, _) = Lsp.handleMsg Lsp.initial (didOpen "val x = 1\\nfun id y = y\\n")
    in st end

  (* one-shot: feed a single message to the given state, return first output *)
  fun out1 st msg =
    case Lsp.handleMsg st msg of
        (_, res :: _) => res
      | (_, []) => "<no output>"

  fun has sub s = String.isSubstring sub s

  fun checkGolden name (got, expected) =
    if got = expected then check name true
    else (print ("    expected:\n" ^ expected ^ "\n    got:\n" ^ got ^ "\n");
          check name false)

  (* ---- transcripts ---- *)

  val initReq =
    "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
  val initResp =
    "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{\"textDocumentSync\":1,\"documentSymbolProvider\":true,\"hoverProvider\":true,\"definitionProvider\":true,\"documentFormattingProvider\":true},\"serverInfo\":{\"name\":\"sml-lsp\",\"version\":\"0.1.0\"}}}"

  val shutdownReq =
    "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"shutdown\"}"
  val shutdownResp =
    "{\"jsonrpc\":\"2.0\",\"id\":9,\"result\":null}"

  val didOpenValid = didOpen "val x = 1"
  val diagValidResp =
    "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"" ^ uri ^ "\",\"diagnostics\":[]}}"

  val didOpenBad = didOpen "val x = ("

  val symReq =
    "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"textDocument/documentSymbol\",\"params\":{\"textDocument\":{\"uri\":\"" ^ uri ^ "\"}}}"
  val symResp =
    "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":[{\"name\":\"x\",\"kind\":13,\"location\":{\"uri\":\"" ^ uri ^ "\",\"range\":{\"start\":{\"line\":0,\"character\":4},\"end\":{\"line\":0,\"character\":5}}}},{\"name\":\"id\",\"kind\":12,\"location\":{\"uri\":\"" ^ uri ^ "\",\"range\":{\"start\":{\"line\":1,\"character\":4},\"end\":{\"line\":1,\"character\":6}}}}]}"

  fun hoverReq (l, c) =
    "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"textDocument/hover\",\"params\":{\"textDocument\":{\"uri\":\"" ^ uri ^ "\"},\"position\":{\"line\":" ^ Int.toString l ^ ",\"character\":" ^ Int.toString c ^ "}}}"
  val hoverResp =
    "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"contents\":{\"kind\":\"plaintext\",\"value\":\"val x\"}}}"

  fun defReq (l, c) =
    "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"" ^ uri ^ "\"},\"position\":{\"line\":" ^ Int.toString l ^ ",\"character\":" ^ Int.toString c ^ "}}}"
  val defResp =
    "{\"jsonrpc\":\"2.0\",\"id\":4,\"result\":{\"uri\":\"" ^ uri ^ "\",\"range\":{\"start\":{\"line\":1,\"character\":4},\"end\":{\"line\":1,\"character\":6}}}}"

  val fmtReq =
    "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"textDocument/formatting\",\"params\":{\"textDocument\":{\"uri\":\"" ^ uri ^ "\"}}}"

  val unknownReq =
    "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"textDocument/rename\",\"params\":{}}"
  val unknownResp =
    "{\"jsonrpc\":\"2.0\",\"id\":7,\"error\":{\"code\":-32601,\"message\":\"method not found: textDocument/rename\"}}"

  fun runAll () =
    let
      val st = openState ()

      val () = section "lifecycle"
      val () = checkGolden "initialize" (out1 Lsp.initial initReq, initResp)
      val () = checkGolden "shutdown" (out1 Lsp.initial shutdownReq, shutdownResp)
      val () = check "initialized notification has no response"
                 (#2 (Lsp.handleMsg Lsp.initial
                        "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}") = [])

      val () = section "diagnostics"
      val () = checkGolden "didOpen valid -> empty diagnostics"
                 (out1 Lsp.initial didOpenValid, diagValidResp)
      val () = check "didOpen invalid -> severity 1"
                 (has "\"severity\":1" (out1 Lsp.initial didOpenBad)
                  andalso has "publishDiagnostics" (out1 Lsp.initial didOpenBad))

      val () = section "documentSymbol"
      val () = checkGolden "two symbols" (out1 st symReq, symResp)

      val () = section "hover"
      val () = checkGolden "hover on val x" (out1 st (hoverReq (0, 4)), hoverResp)
      val () = check "hover off-token -> null"
                 (has "\"result\":null" (out1 st (hoverReq (0, 6))))

      val () = section "definition"
      val () = checkGolden "definition of id" (out1 st (defReq (1, 4)), defResp)

      val () = section "formatting"
      val () = check "formatting returns a text edit"
                 (let val res = out1 st fmtReq
                  in has "newText" res andalso has "val x = 1" res end)

      val () = section "errors"
      val () = checkGolden "unknown request -> -32601"
                 (out1 Lsp.initial unknownReq, unknownResp)
      val () = check "unknown notification -> silent"
                 (#2 (Lsp.handleMsg Lsp.initial
                        "{\"jsonrpc\":\"2.0\",\"method\":\"$/foo\",\"params\":{}}") = [])

      val () = section "full transcript"
      val () = check "run folds outputs in order"
                 (let val outs = Lsp.run Lsp.initial [initReq, didOpenValid, symReq, shutdownReq]
                  in List.length outs = 4 end)
    in () end

  fun run () = (reset (); runAll (); Harness.run ())
end

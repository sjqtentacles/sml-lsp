(* demo.sml - deterministic `make example` asset for sml-lsp.

   Drives the pure server core (`Lsp.run`) over a recorded JSON-RPC transcript
   and prints each serialized response, then exercises diagnostics on a file
   with a syntax error. No stdio loop, no clock, no randomness. *)

val uri = "file:///demo.sml"

fun didOpen text =
  "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":"
  ^ "{\"textDocument\":{\"uri\":\"" ^ uri ^ "\",\"text\":\"" ^ text ^ "\"}}}"

val transcript =
  [ "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
  , didOpen "val answer = 42\\nfun inc n = n + 1\\n"
  , "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"textDocument/documentSymbol\",\"params\":{\"textDocument\":{\"uri\":\"" ^ uri ^ "\"}}}"
  , "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"textDocument/hover\",\"params\":{\"textDocument\":{\"uri\":\"" ^ uri ^ "\"},\"position\":{\"line\":1,\"character\":4}}}"
  , "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"shutdown\"}" ]

val () = print "=== JSON-RPC transcript (responses) ===\n"
val outs = Lsp.run Lsp.initial transcript
val () = List.app (fn s => print (s ^ "\n")) outs

val () = print "\n=== diagnostics on a broken document ===\n"
val (_, diags) = Lsp.handleMsg Lsp.initial (didOpen "val x = (")
val () = List.app (fn s => print (s ^ "\n")) diags

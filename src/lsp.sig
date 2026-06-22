(* lsp.sig

   A Language Server Protocol server for Standard ML over JSON-RPC. The pure
   request/response handlers (`handleMsg`, `run`) are driven by recorded
   JSON-RPC transcripts and are fully deterministic; `serve` is the only impure
   shell (the Content-Length framed stdio loop), kept thin.

   Capabilities: diagnostics (parse errors via sml-mlast), documentSymbol,
   hover, go-to-definition, and document formatting (delegated to sml-fmt). *)

signature LSP =
sig
  type state
  val initial : state

  (* Process one JSON-RPC message (request or notification) given as text;
     return the updated state and the serialized output messages (responses
     and/or notifications), in order. Pure. *)
  val handleMsg : state -> string -> state * string list

  (* Fold handleMsg over a transcript, returning all outputs in order. The
     testable pure core. *)
  val run : state -> string list -> string list

  (* Impure stdio shell: read Content-Length framed JSON-RPC from stdin, write
     framed responses to stdout, until `exit`. *)
  val serve : unit -> unit
end

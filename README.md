# sml-lsp

> A pure Standard ML language server (LSP over JSON-RPC) for Standard ML.

[![CI](https://github.com/sjqtentacles/sml-lsp/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-lsp/actions)

A **Language Server Protocol** server for Standard ML, speaking JSON-RPC over
stdio. The protocol logic is a **pure** function from an incoming JSON-RPC
message to the serialized outgoing messages, so it is fully deterministic and
testable with recorded transcripts; the only impure part is a thin
`Content-Length`-framed stdio loop (`serve`). Byte-identical under
[MLton](http://mlton.org/) and [Poly/ML](https://www.polyml.org/).

It vendors [`sml-mlast`](https://github.com/sjqtentacles/sml-mlast) (parser /
diagnostics), [`sml-fmt`](https://github.com/sjqtentacles/sml-fmt) (formatting),
and [`sml-json`](https://github.com/sjqtentacles/sml-json) (JSON-RPC encoding).

## Capabilities

| LSP request                    | Behaviour |
| ------------------------------ | --------- |
| `initialize` / `shutdown`      | lifecycle handshake, advertises capabilities |
| `textDocument/didOpen`/`didChange` | stores the document, publishes diagnostics |
| `textDocument/publishDiagnostics`  | parse errors from `sml-mlast` (severity 1) |
| `textDocument/documentSymbol`  | top-level `val`/`fun`/`type`/`structure`/… symbols |
| `textDocument/hover`           | kind + name of the identifier under the cursor |
| `textDocument/definition`      | jumps to the declaring occurrence in the file |
| `textDocument/formatting`      | a whole-document edit produced by `sml-fmt` |

## API

```sml
type state
val initial   : state
val handleMsg : state -> string -> state * string list  (* pure core *)
val run       : state -> string list -> string list     (* fold a transcript *)
val serve     : unit -> unit                             (* impure stdio loop *)
```

## Install (smlpkg)

```sh
smlpkg add github.com/sjqtentacles/sml-lsp
```

or add it to your package's `sml.pkg`:

```
require {
  github.com/sjqtentacles/sml-lsp
}
```

## Build & test

```sh
make test        # MLton: build + run the JSON-RPC transcript suite
make test-poly   # Poly/ML: run the same suite
make all-tests   # both compilers
make example     # run a sample transcript through the server core
```

Both compilers report `13 passed, 0 failed` with byte-identical output. The
suite pins golden JSON-RPC responses for the lifecycle, diagnostics,
documentSymbol, hover, go-to-definition, formatting, and error handling.

## Example

`make example` drives the pure core over a recorded transcript:

```
=== JSON-RPC transcript (responses) ===
{"jsonrpc":"2.0","id":1,"result":{"capabilities":{"textDocumentSync":1,"documentSymbolProvider":true,"hoverProvider":true,"definitionProvider":true,"documentFormattingProvider":true},"serverInfo":{"name":"sml-lsp","version":"0.1.0"}}}
{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"file:///demo.sml","diagnostics":[]}}
{"jsonrpc":"2.0","id":2,"result":[{"name":"answer","kind":13,"location":{"uri":"file:///demo.sml","range":{"start":{"line":0,"character":4},"end":{"line":0,"character":10}}}},{"name":"inc","kind":12,"location":{"uri":"file:///demo.sml","range":{"start":{"line":1,"character":4},"end":{"line":1,"character":7}}}}]}
{"jsonrpc":"2.0","id":3,"result":{"contents":{"kind":"plaintext","value":"fun inc"}}}
{"jsonrpc":"2.0","id":4,"result":null}

=== diagnostics on a broken document ===
{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"file:///demo.sml","diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":1,"source":"sml-mlast","message":"expected expression, got <eof>"}]}}
```

## Layout

Layout B (vendoring): own sources live in `src/`; the
[`sml-mlast`](https://github.com/sjqtentacles/sml-mlast),
[`sml-fmt`](https://github.com/sjqtentacles/sml-fmt),
[`sml-json`](https://github.com/sjqtentacles/sml-json), and transitive
`sml-parsec` trees are vendored under `lib/github.com/sjqtentacles/` and loaded
first. Positions are derived from a lightweight source scan (the AST is
position-free), so symbol/hover/definition lookups are name-based within a file.

## License

MIT — see [LICENSE](LICENSE).

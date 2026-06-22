(* fmt.sml - see fmt.sig *)

structure Format :> FORMAT =
struct
  fun string s = PpAst.ppProgram (Parser.parseString s) ^ "\n"

  fun file path =
    let
      val ins = TextIO.openIn path
      val s = TextIO.inputAll ins
      val () = TextIO.closeIn ins
    in string s end
end

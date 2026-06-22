(* parser.sml - see parser.sig *)

structure Parser :> PARSER =
struct
  open Ast
  exception Parse of string

  datatype assoc = LeftA | RightA

  val defaultFixity : (string * (int * assoc)) list =
    [ ("/", (7, LeftA)), ("*", (7, LeftA)), ("div", (7, LeftA)),
      ("mod", (7, LeftA)),
      ("+", (6, LeftA)), ("-", (6, LeftA)), ("^", (6, LeftA)),
      ("::", (5, RightA)), ("@", (5, RightA)),
      ("=", (4, LeftA)), ("<>", (4, LeftA)), ("<", (4, LeftA)),
      (">", (4, LeftA)), ("<=", (4, LeftA)), (">=", (4, LeftA)),
      (":=", (3, LeftA)), ("o", (3, LeftA)),
      ("before", (0, LeftA)) ]

  (* mutable parse state *)
  val toksRef = ref (Vector.fromList ([] : Token.token list))
  val posRef = ref 0
  val fixityRef = ref defaultFixity

  fun peek () = Vector.sub (!toksRef, !posRef) handle Subscript => Token.EOF
  fun peekN k = Vector.sub (!toksRef, !posRef + k) handle Subscript => Token.EOF
  fun adv () = posRef := !posRef + 1
  fun expect t =
    if peek () = t then adv ()
    else raise Parse ("expected " ^ Token.toString t ^ ", got "
                      ^ Token.toString (peek ()))

  fun lookupFix s =
    case List.find (fn (k, _) => k = s) (!fixityRef) of
        SOME (_, f) => SOME f
      | NONE => NONE
  fun isInfix s = Option.isSome (lookupFix s)
  fun addFixity (s, prec, assoc) =
    fixityRef := (s, (prec, assoc)) :: List.filter (fn (k, _) => k <> s) (!fixityRef)
  fun removeFixity s =
    fixityRef := List.filter (fn (k, _) => k <> s) (!fixityRef)

  fun peekInfixOp () =
    case peek () of
        Token.ID s => (case lookupFix s of SOME f => SOME (s, f) | NONE => NONE)
      | Token.EQUALS => SOME ("=", (4, LeftA))
      | _ => NONE

  fun atexpStarts t =
    case t of
        Token.ID s => not (isInfix s)
      | Token.OP => true
      | Token.INT _ => true | Token.WORD _ => true | Token.REAL _ => true
      | Token.STRING _ => true | Token.CHAR _ => true
      | Token.LPAREN => true | Token.LBRACK => true | Token.LBRACE => true
      | Token.HASH => true | Token.LET => true
      | _ => false

  fun patAtomStarts t =
    case t of
        Token.ID s => not (isInfix s)
      | Token.UNDERSCORE => true
      | Token.OP => true
      | Token.INT _ => true | Token.WORD _ => true | Token.REAL _ => true
      | Token.STRING _ => true | Token.CHAR _ => true
      | Token.LPAREN => true | Token.LBRACK => true | Token.LBRACE => true
      | _ => false

  fun startsDec t =
    case t of
        Token.VAL => true | Token.FUN => true | Token.TYPE => true
      | Token.DATATYPE => true | Token.EXCEPTION => true | Token.OPEN => true
      | Token.LOCAL => true | Token.INFIX => true | Token.INFIXR => true
      | Token.NONFIX => true | Token.STRUCTURE => true | Token.SIGNATURE => true
      | Token.FUNCTOR => true
      | _ => false

  fun skipSemis () =
    case peek () of Token.SEMICOLON => (adv (); skipSemis ()) | _ => ()

  (* ---- the recursive grammar ---- *)

  fun exp () =
    case peek () of
        Token.FN => (adv (); EFn (parseMatch ()))
      | Token.CASE =>
          (adv ();
           let val e = exp ()
           in expect Token.OF; ECase (e, parseMatch ()) end)
      | Token.IF =>
          (adv ();
           let val c = exp ()
           in expect Token.THEN;
              let val t = exp ()
              in expect Token.ELSE; EIf (c, t, exp ()) end
           end)
      | Token.WHILE =>
          (adv ();
           let val c = exp ()
           in expect Token.DO; EWhile (c, exp ()) end)
      | Token.RAISE => (adv (); ERaise (exp ()))
      | _ => expHandle ()

  and expHandle () =
    let val e = expOr ()
    in case peek () of
           Token.HANDLE => (adv (); EHandle (e, parseMatch ()))
         | _ => e
    end

  and expOr () =
    let val e = expAnd ()
    in case peek () of Token.ORELSE => (adv (); EOrelse (e, exp ())) | _ => e end

  and expAnd () =
    let val e = expTyped ()
    in case peek () of Token.ANDALSO => (adv (); EAndalso (e, exp ())) | _ => e end

  and expTyped () =
    let val e = infexp ()
    in case peek () of Token.COLON => (adv (); ETyped (e, ty ())) | _ => e end

  and infexp () =
    let
      fun climb minPrec =
        let
          fun loop left =
            case peekInfixOp () of
                SOME (opid, (prec, assoc)) =>
                  if prec < minPrec then left
                  else
                    (adv ();
                     let
                       val nextMin = case assoc of LeftA => prec + 1 | RightA => prec
                       val right = climb nextMin
                     in loop (EInfix (opid, left, right)) end)
              | NONE => left
        in loop (appexp ()) end
    in climb 0 end

  and appexp () =
    let
      fun loop e =
        if atexpStarts (peek ()) then loop (EApp (e, atexp ())) else e
    in loop (atexp ()) end

  and atexp () =
    case peek () of
        Token.INT s => (adv (); ELit (LInt s))
      | Token.WORD s => (adv (); ELit (LWord s))
      | Token.REAL s => (adv (); ELit (LReal s))
      | Token.STRING s => (adv (); ELit (LString s))
      | Token.CHAR s => (adv (); ELit (LChar s))
      | Token.ID s => (adv (); EVar s)
      | Token.OP =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); EVar s)
             | Token.EQUALS => (adv (); EVar "=")
             | t => raise Parse ("expected identifier after op, got "
                                 ^ Token.toString t))
      | Token.HASH =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); ESelector s)
             | Token.INT s => (adv (); ESelector s)
             | t => raise Parse ("expected label after #, got "
                                 ^ Token.toString t))
      | Token.LET => parseLet ()
      | Token.LPAREN => parseParenExp ()
      | Token.LBRACK => parseListExp ()
      | Token.LBRACE => parseRecordExp ()
      | t => raise Parse ("expected expression, got " ^ Token.toString t)

  and parseLet () =
    (expect Token.LET;
     let val ds = parseDecs (fn t => t = Token.IN)
     in expect Token.IN;
        let
          fun seqRest acc =
            case peek () of
                Token.SEMICOLON => (adv (); seqRest (exp () :: acc))
              | _ => List.rev acc
          val es = seqRest [exp ()]
        in expect Token.END;
           ELet (ds, case es of [e] => e | _ => ESeq es)
        end
     end)

  and parseParenExp () =
    (expect Token.LPAREN;
     case peek () of
         Token.RPAREN => (adv (); ETuple [])
       | _ =>
           let val e1 = exp ()
           in case peek () of
                  Token.RPAREN => (adv (); e1)
                | Token.COMMA =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.COMMA => (adv (); rest (exp () :: acc))
                          | _ => List.rev acc
                      val es = rest [e1]
                    in expect Token.RPAREN; ETuple es end
                | Token.SEMICOLON =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.SEMICOLON => (adv (); rest (exp () :: acc))
                          | _ => List.rev acc
                      val es = rest [e1]
                    in expect Token.RPAREN; ESeq es end
                | t => raise Parse ("expected ) , or ; got " ^ Token.toString t)
           end)

  and parseListExp () =
    (expect Token.LBRACK;
     case peek () of
         Token.RBRACK => (adv (); EList [])
       | _ =>
           let
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (exp () :: acc))
                 | _ => List.rev acc
             val es = rest [exp ()]
           in expect Token.RBRACK; EList es end)

  and parseRecordExp () =
    (expect Token.LBRACE;
     case peek () of
         Token.RBRACE => (adv (); ERecord [])
       | _ =>
           let
             fun field () =
               let val lab = parseLabel ()
               in expect Token.EQUALS; (lab, exp ()) end
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (field () :: acc))
                 | _ => List.rev acc
             val fs = rest [field ()]
           in expect Token.RBRACE; ERecord fs end)

  and parseLabel () =
    case peek () of
        Token.ID s => (adv (); s)
      | Token.INT s => (adv (); s)
      | t => raise Parse ("expected label, got " ^ Token.toString t)

  and parseMatch () =
    let
      fun arm () =
        let val p = pat ()
        in expect Token.DARROW; (p, exp ()) end
      fun loop acc =
        let val a = arm ()
        in case peek () of Token.BAR => (adv (); loop (a :: acc))
                         | _ => List.rev (a :: acc)
        end
    in loop [] end

  (* ---- patterns ---- *)

  and pat () =
    let val p = patInfix ()
    in case peek () of
           Token.COLON => (adv (); PTyped (p, ty ()))
         | Token.AS =>
             (case p of
                  PVar id => (adv (); PAs (id, pat ()))
                | _ => raise Parse "as requires a variable")
         | _ => p
    end

  and patInfix () =
    let
      fun climb minPrec =
        let
          fun loop left =
            case peek () of
                Token.ID s =>
                  (case lookupFix s of
                       SOME (prec, assoc) =>
                         if prec < minPrec then left
                         else
                           (adv ();
                            let
                              val nextMin =
                                case assoc of LeftA => prec + 1 | RightA => prec
                              val right = climb nextMin
                            in loop (PInfix (s, left, right)) end)
                     | NONE => left)
              | _ => left
        in loop (patApp ()) end
    in climb 0 end

  and patApp () =
    let val p = patAtom ()
    in case p of
           PVar id =>
             if patAtomStarts (peek ()) then PCon (id, patAtom ()) else p
         | _ => p
    end

  and patAtom () =
    case peek () of
        Token.UNDERSCORE => (adv (); PWild)
      | Token.ID s => (adv (); PVar s)
      | Token.OP =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); PVar s)
             | Token.EQUALS => (adv (); PVar "=")
             | t => raise Parse ("expected id after op, got " ^ Token.toString t))
      | Token.INT s => (adv (); PLit (LInt s))
      | Token.WORD s => (adv (); PLit (LWord s))
      | Token.REAL s => (adv (); PLit (LReal s))
      | Token.STRING s => (adv (); PLit (LString s))
      | Token.CHAR s => (adv (); PLit (LChar s))
      | Token.LPAREN => parseParenPat ()
      | Token.LBRACK => parseListPat ()
      | Token.LBRACE => parseRecordPat ()
      | t => raise Parse ("expected pattern, got " ^ Token.toString t)

  and parseParenPat () =
    (expect Token.LPAREN;
     case peek () of
         Token.RPAREN => (adv (); PTuple [])
       | _ =>
           let val p1 = pat ()
           in case peek () of
                  Token.RPAREN => (adv (); p1)
                | Token.COMMA =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.COMMA => (adv (); rest (pat () :: acc))
                          | _ => List.rev acc
                      val ps = rest [p1]
                    in expect Token.RPAREN; PTuple ps end
                | t => raise Parse ("expected ) or , in pattern, got "
                                    ^ Token.toString t)
           end)

  and parseListPat () =
    (expect Token.LBRACK;
     case peek () of
         Token.RBRACK => (adv (); PList [])
       | _ =>
           let
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (pat () :: acc))
                 | _ => List.rev acc
             val ps = rest [pat ()]
           in expect Token.RBRACK; PList ps end)

  and parseRecordPat () =
    (expect Token.LBRACE;
     let
       fun finish (acc, flex) =
         (expect Token.RBRACE; PRecord (List.rev acc, flex))
       fun fields (acc, flex) =
         case peek () of
             Token.RBRACE => finish (acc, flex)
           | Token.DOTDOTDOT => (adv (); finish (acc, true))
           | _ =>
               let
                 val lab = parseLabel ()
                 val p = case peek () of
                             Token.EQUALS => (adv (); pat ())
                           | _ => PVar lab
               in case peek () of
                      Token.COMMA => (adv (); fields ((lab, p) :: acc, flex))
                    | _ => finish ((lab, p) :: acc, flex)
               end
     in fields ([], false) end)

  (* ---- types ---- *)

  and ty () = tyArrow ()

  and tyArrow () =
    let val t = tyTuple ()
    in case peek () of Token.ARROW => (adv (); TyArrow (t, ty ())) | _ => t end

  and tyTuple () =
    let
      val t = tyApp ()
      fun loop acc =
        case peek () of
            Token.ID "*" => (adv (); loop (tyApp () :: acc))
          | _ => List.rev acc
      val rest = loop [t]
    in case rest of [single] => single | many => TyTuple many end

  and tyApp () =
    let
      fun loop t =
        case peek () of
            Token.ID s =>
              if s <> "*" then (adv (); loop (TyCon ([t], s))) else t
          | _ => t
    in loop (tyAtom ()) end

  and tyAtom () =
    case peek () of
        Token.TYVAR s => (adv (); TyVar s)
      | Token.ID s => (adv (); TyCon ([], s))
      | Token.LBRACE => parseRecordTy ()
      | Token.LPAREN =>
          (adv ();
           let val t = ty ()
           in case peek () of
                  Token.RPAREN => (adv (); t)
                | Token.COMMA =>
                    let
                      fun rest acc =
                        case peek () of
                            Token.COMMA => (adv (); rest (ty () :: acc))
                          | _ => List.rev acc
                      val ts = rest [t]
                    in expect Token.RPAREN;
                       case peek () of
                           Token.ID s => (adv (); TyCon (ts, s))
                         | tk => raise Parse ("expected type constructor, got "
                                              ^ Token.toString tk)
                    end
                | tk => raise Parse ("expected ) or , in type, got "
                                     ^ Token.toString tk)
           end)
      | t => raise Parse ("expected type, got " ^ Token.toString t)

  and parseRecordTy () =
    (expect Token.LBRACE;
     case peek () of
         Token.RBRACE => (adv (); TyRecord [])
       | _ =>
           let
             fun field () =
               let val lab = parseLabel ()
               in expect Token.COLON; (lab, ty ()) end
             fun rest acc =
               case peek () of
                   Token.COMMA => (adv (); rest (field () :: acc))
                 | _ => List.rev acc
             val fs = rest [field ()]
           in expect Token.RBRACE; TyRecord fs end)

  and parseTyvarSeq () =
    case peek () of
        Token.TYVAR s => (adv (); [s])
      | Token.LPAREN =>
          (case peekN 1 of
               Token.TYVAR _ =>
                 (adv ();
                  let
                    fun loop acc =
                      case peek () of
                          Token.TYVAR s =>
                            (adv ();
                             case peek () of
                                 Token.COMMA => (adv (); loop (s :: acc))
                               | _ => List.rev (s :: acc))
                        | _ => List.rev acc
                    val tvs = loop []
                  in expect Token.RPAREN; tvs end)
             | _ => [])
      | _ => []

  (* ---- declarations ---- *)

  and parseDecs stop =
    let
      fun loop acc =
        if peek () = Token.EOF orelse stop (peek ()) then List.rev acc
        else
          let val d = parseDec ()
          in skipSemis (); loop (d :: acc) end
    in loop [] end

  and parseDec () =
    case peek () of
        Token.VAL => parseVal ()
      | Token.FUN => parseFun ()
      | Token.TYPE => parseTypeDec ()
      | Token.DATATYPE => parseDatatype ()
      | Token.EXCEPTION => parseException ()
      | Token.OPEN => parseOpen ()
      | Token.LOCAL => parseLocal ()
      | Token.INFIX => parseInfix false
      | Token.INFIXR => parseInfix true
      | Token.NONFIX => parseNonfix ()
      | Token.STRUCTURE => parseStructure ()
      | Token.SIGNATURE => parseSignature ()
      | Token.FUNCTOR => parseFunctor ()
      | t => raise Parse ("expected declaration, got " ^ Token.toString t)

  and parseVal () =
    (expect Token.VAL;
     let
       val tvs = parseTyvarSeq ()
       val isRec = case peek () of Token.REC => (adv (); true) | _ => false
       fun binding () =
         let val p = pat ()
         in expect Token.EQUALS; (p, exp ()) end
       fun loop acc =
         let val b = binding ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in DVal (tvs, loop [], isRec) end)

  and parseFun () =
    (expect Token.FUN;
     let
       val tvs = parseTyvarSeq ()
       fun clause () =
         let
           val name = parseVid ()
           fun args acc =
             if patAtomStarts (peek ()) then args (patAtom () :: acc)
             else List.rev acc
           val ps = args []
           val ret = case peek () of Token.COLON => (adv (); SOME (ty ()))
                                   | _ => NONE
           val _ = expect Token.EQUALS
           val body = exp ()
         in (name, { pats = ps, ret = ret, body = body }) end
       fun clauses (nm, cs) =
         case peek () of
             Token.BAR =>
               (adv ();
                let val (_, c) = clause ()
                in clauses (nm, c :: cs) end)
           | _ => (nm, List.rev cs)
       fun functions acc =
         let
           val (nm, c) = clause ()
           val fdef = clauses (nm, [c])
         in case peek () of
                Token.AND => (adv (); functions (fdef :: acc))
              | _ => List.rev (fdef :: acc)
         end
     in DFun (tvs, functions []) end)

  and parseVid () =
    case peek () of
        Token.ID s => (adv (); s)
      | Token.OP =>
          (adv ();
           case peek () of
               Token.ID s => (adv (); s)
             | Token.EQUALS => (adv (); "=")
             | t => raise Parse ("expected id after op, got " ^ Token.toString t))
      | t => raise Parse ("expected identifier, got " ^ Token.toString t)

  and parseTyConName () =
    case peek () of
        Token.ID s => (adv (); s)
      | t => raise Parse ("expected type constructor name, got "
                          ^ Token.toString t)

  and parseTypeDec () =
    (expect Token.TYPE;
     let
       fun bind () =
         let
           val tvs = parseTyvarSeq ()
           val name = parseTyConName ()
           val _ = expect Token.EQUALS
         in (tvs, name, ty ()) end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in DType (loop []) end)

  and parseDatbinds () =
    let
      fun con () =
        let val c = parseVid ()
        in case peek () of Token.OF => (adv (); (c, SOME (ty ())))
                         | _ => (c, NONE)
        end
      fun cons acc =
        let val c = con ()
        in case peek () of Token.BAR => (adv (); cons (c :: acc))
                         | _ => List.rev (c :: acc)
        end
      fun bind () =
        let
          val tvs = parseTyvarSeq ()
          val name = parseTyConName ()
          val _ = expect Token.EQUALS
        in { tyvars = tvs, name = name, cons = cons [] } end
      fun loop acc =
        let val b = bind ()
        in case peek () of Token.AND => (adv (); loop (b :: acc))
                         | _ => List.rev (b :: acc)
        end
    in loop [] end

  and parseDatatype () =
    (expect Token.DATATYPE;
     let
       val dbs = parseDatbinds ()
       val withs =
         case peek () of
             Token.WITHTYPE =>
               (adv ();
                let
                  fun tb () =
                    let
                      val tvs = parseTyvarSeq ()
                      val nm = parseTyConName ()
                      val _ = expect Token.EQUALS
                    in (tvs, nm, ty ()) end
                  fun loop acc =
                    let val b = tb ()
                    in case peek () of Token.AND => (adv (); loop (b :: acc))
                                     | _ => List.rev (b :: acc)
                    end
                in loop [] end)
           | _ => []
     in DDatatype (dbs, withs) end)

  and parseException () =
    (expect Token.EXCEPTION;
     let
       fun eb () =
         let val c = parseVid ()
         in case peek () of Token.OF => (adv (); (c, SOME (ty ())))
                          | _ => (c, NONE)
         end
       fun loop acc =
         let val b = eb ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in DException (loop []) end)

  and parseOpen () =
    (expect Token.OPEN;
     let
       fun loop acc =
         case peek () of Token.ID s => (adv (); loop (s :: acc))
                       | _ => List.rev acc
       val ids = loop []
     in if null ids then raise Parse "open needs an identifier"
        else DOpen ids
     end)

  and parseLocal () =
    (expect Token.LOCAL;
     let val d1 = parseDecs (fn t => t = Token.IN)
     in expect Token.IN;
        let val d2 = parseDecs (fn t => t = Token.END)
        in expect Token.END; DLocal (d1, d2) end
     end)

  and parseInfix isRight =
    (adv ();
     let
       val prec = case peek () of
                      Token.INT s => (adv (); valOf (Int.fromString s))
                    | _ => 0
       fun loop acc =
         case peek () of
             Token.ID s => (adv (); loop (s :: acc))
           | Token.EQUALS => (adv (); loop ("=" :: acc))
           | _ => List.rev acc
       val ids = loop []
       val assoc = if isRight then RightA else LeftA
       val () = List.app (fn id => addFixity (id, prec, assoc)) ids
     in if isRight then DInfixr (prec, ids) else DInfix (prec, ids) end)

  and parseNonfix () =
    (expect Token.NONFIX;
     let
       fun loop acc =
         case peek () of
             Token.ID s => (adv (); loop (s :: acc))
           | Token.EQUALS => (adv (); loop ("=" :: acc))
           | _ => List.rev acc
       val ids = loop []
       val () = List.app removeFixity ids
     in DNonfix ids end)

  and parseStructure () =
    (expect Token.STRUCTURE;
     let
       fun bind () =
         let
           val name = parseTyConName ()
           val asc = case peek () of
                         Token.COLON => (adv (); SOME (false, sigexp ()))
                       | Token.COLONGT => (adv (); SOME (true, sigexp ()))
                       | _ => NONE
           val _ = expect Token.EQUALS
           val body = strexp ()
           val body2 = case asc of
                           SOME (opq, se) => StrConstraint (body, se, opq)
                         | NONE => body
         in (name, body2) end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in DStructure (loop []) end)

  and strexp () =
    let
      fun base () =
        case peek () of
            Token.STRUCT =>
              (adv ();
               let val ds = parseDecs (fn t => t = Token.END)
               in expect Token.END; StrStruct ds end)
          | Token.LET =>
              (adv ();
               let val ds = parseDecs (fn t => t = Token.IN)
               in expect Token.IN;
                  let val se = strexp ()
                  in expect Token.END; StrLet (ds, se) end
               end)
          | Token.ID s =>
              (adv ();
               case peek () of
                   Token.LPAREN =>
                     (adv ();
                      let
                        val arg =
                          if startsDec (peek ()) then
                            StrStruct (parseDecs (fn t => t = Token.RPAREN))
                          else strexp ()
                      in expect Token.RPAREN; StrApp (s, arg) end)
                 | _ => StrId s)
          | t => raise Parse ("expected structure expression, got "
                              ^ Token.toString t)
      fun loop se =
        case peek () of
            Token.COLON => (adv (); loop (StrConstraint (se, sigexp (), false)))
          | Token.COLONGT => (adv (); loop (StrConstraint (se, sigexp (), true)))
          | _ => se
    in loop (base ()) end

  and parseSignature () =
    (expect Token.SIGNATURE;
     let
       fun bind () =
         let
           val name = parseTyConName ()
           val _ = expect Token.EQUALS
         in (name, sigexp ()) end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in DSignature (loop []) end)

  and sigexp () =
    let
      fun base () =
        case peek () of
            Token.SIG =>
              (adv ();
               let val sps = parseSpecs ()
               in expect Token.END; SigSig sps end)
          | Token.ID s => (adv (); SigId s)
          | t => raise Parse ("expected signature expression, got "
                              ^ Token.toString t)
      fun loop se =
        case peek () of
            Token.WHERE =>
              (adv (); expect Token.TYPE;
               let
                 val tvs = parseTyvarSeq ()
                 val nm = parseTyConName ()
                 val _ = expect Token.EQUALS
                 val t = ty ()
               in loop (SigWhere (se, [(tvs, nm, t)])) end)
          | _ => se
    in loop (base ()) end

  and parseSpecs () =
    let
      fun loop acc =
        if peek () = Token.EOF orelse peek () = Token.END then List.rev acc
        else let val s = parseSpec () in skipSemis (); loop (s :: acc) end
    in loop [] end

  and parseSpec () =
    case peek () of
        Token.VAL =>
          (adv ();
           let
             fun vb () =
               let val v = parseVid ()
               in expect Token.COLON; (v, ty ()) end
             fun loop acc =
               let val b = vb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in SpecVal (loop []) end)
      | Token.TYPE =>
          (adv ();
           let
             fun tb () =
               let
                 val tvs = parseTyvarSeq ()
                 val nm = parseTyConName ()
               in case peek () of
                      Token.EQUALS => (adv (); (tvs, nm, SOME (ty ())))
                    | _ => (tvs, nm, NONE)
               end
             fun loop acc =
               let val b = tb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
             val bs = loop []
             val allDef = List.all (fn (_, _, x) => Option.isSome x) bs
           in
             if allDef then
               SpecTypeDef (List.map (fn (tv, n, x) => (tv, n, valOf x)) bs)
             else SpecType (List.map (fn (tv, n, _) => (tv, n)) bs)
           end)
      | Token.EQTYPE =>
          (adv ();
           let
             fun tb () =
               let val tvs = parseTyvarSeq () in (tvs, parseTyConName ()) end
             fun loop acc =
               let val b = tb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in SpecEqtype (loop []) end)
      | Token.DATATYPE => (adv (); SpecDatatype (parseDatbinds ()))
      | Token.EXCEPTION =>
          (adv ();
           let
             fun eb () =
               let val c = parseVid ()
               in case peek () of Token.OF => (adv (); (c, SOME (ty ())))
                                | _ => (c, NONE)
               end
             fun loop acc =
               let val b = eb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in SpecException (loop []) end)
      | Token.STRUCTURE =>
          (adv ();
           let
             fun sb () =
               let val nm = parseTyConName ()
               in expect Token.COLON; (nm, sigexp ()) end
             fun loop acc =
               let val b = sb ()
               in case peek () of Token.AND => (adv (); loop (b :: acc))
                                | _ => List.rev (b :: acc)
               end
           in SpecStructure (loop []) end)
      | Token.INCLUDE => (adv (); SpecInclude (sigexp ()))
      | t => raise Parse ("expected specification, got " ^ Token.toString t)

  and parseFunctor () =
    (expect Token.FUNCTOR;
     let
       fun bind () =
         let
           val name = parseTyConName ()
           val _ = expect Token.LPAREN
           val argName = parseTyConName ()
           val _ = expect Token.COLON
           val argSig = sigexp ()
           val _ = expect Token.RPAREN
           val asc = case peek () of
                         Token.COLON => (adv (); SOME (false, sigexp ()))
                       | Token.COLONGT => (adv (); SOME (true, sigexp ()))
                       | _ => NONE
           val _ = expect Token.EQUALS
           val body = strexp ()
         in { name = name, arg = argName, argSig = argSig,
              ascription = asc, body = body } end
       fun loop acc =
         let val b = bind ()
         in case peek () of Token.AND => (adv (); loop (b :: acc))
                          | _ => List.rev (b :: acc)
         end
     in DFunctor (loop []) end)

  (* ---- entry points ---- *)

  fun reset tokens =
    (toksRef := Vector.fromList tokens; posRef := 0;
     fixityRef := defaultFixity)

  fun parse tokens =
    let
      val () = reset tokens
      val ds = parseDecs (fn _ => false)
    in case peek () of
           Token.EOF => ds
         | t => raise Parse ("trailing input: " ^ Token.toString t)
    end

  fun parseString s = parse (Lexer.tokenize s)

  fun parseExp s =
    let
      val () = reset (Lexer.tokenize s)
      val e = exp ()
    in case peek () of
           Token.EOF => e
         | t => raise Parse ("trailing input: " ^ Token.toString t)
    end
end

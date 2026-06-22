(* ast.sml - see ast.sig. Transparent ascription keeps constructors visible. *)

structure Ast : AST =
struct
  type tyvar = string
  type ident = string

  datatype lit =
      LInt of string
    | LWord of string
    | LReal of string
    | LChar of string
    | LString of string

  datatype ty =
      TyVar of tyvar
    | TyCon of ty list * ident
    | TyTuple of ty list
    | TyArrow of ty * ty
    | TyRecord of (string * ty) list

  datatype pat =
      PWild
    | PVar of ident
    | PLit of lit
    | PTuple of pat list
    | PList of pat list
    | PRecord of (string * pat) list * bool
    | PCon of ident * pat
    | PInfix of ident * pat * pat
    | PTyped of pat * ty
    | PAs of ident * pat

  datatype exp =
      ELit of lit
    | EVar of ident
    | ETuple of exp list
    | EList of exp list
    | ERecord of (string * exp) list
    | ESelector of string
    | ESeq of exp list
    | EApp of exp * exp
    | EInfix of ident * exp * exp
    | ETyped of exp * ty
    | EAndalso of exp * exp
    | EOrelse of exp * exp
    | EHandle of exp * (pat * exp) list
    | ERaise of exp
    | EIf of exp * exp * exp
    | EWhile of exp * exp
    | ECase of exp * (pat * exp) list
    | EFn of (pat * exp) list
    | ELet of dec list * exp

  and dec =
      DVal of tyvar list * (pat * exp) list * bool
    | DFun of tyvar list
              * (ident * { pats : pat list, ret : ty option, body : exp } list) list
    | DType of (tyvar list * ident * ty) list
    | DDatatype of
        { tyvars : tyvar list, name : ident, cons : (ident * ty option) list } list
        * (tyvar list * ident * ty) list
    | DException of (ident * ty option) list
    | DOpen of ident list
    | DLocal of dec list * dec list
    | DInfix of int * ident list
    | DInfixr of int * ident list
    | DNonfix of ident list
    | DStructure of (ident * strexp) list
    | DSignature of (ident * sigexp) list
    | DFunctor of
        { name : ident, arg : ident, argSig : sigexp,
          ascription : (bool * sigexp) option, body : strexp } list

  and strexp =
      StrStruct of dec list
    | StrId of ident
    | StrApp of ident * strexp
    | StrLet of dec list * strexp
    | StrConstraint of strexp * sigexp * bool

  and sigexp =
      SigSig of spec list
    | SigId of ident
    | SigWhere of sigexp * (tyvar list * ident * ty) list

  and spec =
      SpecVal of (ident * ty) list
    | SpecType of (tyvar list * ident) list
    | SpecEqtype of (tyvar list * ident) list
    | SpecTypeDef of (tyvar list * ident * ty) list
    | SpecDatatype of
        { tyvars : tyvar list, name : ident, cons : (ident * ty option) list } list
    | SpecException of (ident * ty option) list
    | SpecStructure of (ident * sigexp) list
    | SpecInclude of sigexp

  type fclause = { pats : pat list, ret : ty option, body : exp }
  type datbind = { tyvars : tyvar list, name : ident,
                   cons : (ident * ty option) list }
  type fctbind = { name : ident, arg : ident, argSig : sigexp,
                   ascription : (bool * sigexp) option, body : strexp }

  type program = dec list
end

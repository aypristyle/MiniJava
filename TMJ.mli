(** This is the same abstract syntax tree as in [LMJ.mli] but without position informations.
    After typechecking, we don't need to give feedbacks to the user. *)

type identifier = string

type expression = { raw_expression : raw_expression; typ : typ }

and raw_expression =
  | EConst of constant
  | EGetVar of identifier
  | EUnOp of unop * expression
  | EBinOp of binop * expression * expression
  | EMethodCall of expression * identifier * expression list
  | EArrayGet of expression * expression
  | EArrayAlloc of expression
  | EArrayLength of expression
  | EThis
  | EObjectAlloc of identifier

and constant = LMJ.constant =
  | ConstBool of bool (** Boolean constant [true] or [false]. *)
  | ConstInt of int32 (** Integer constant [[-2^31, 2^31 - 1]]. *)
  | ConstKm of int32 (** Km constant**)
  | ConstMm of int32 (** Mm constant **)
  | ConstCm of int32 (** Cm constant **)
  | Constm of int32 (** m constant **)
  | ConstKg of int32 (** Kg constant**)
  | ConstMg of int32 (** Mg constant **)
  | ConstCg of int32 (** Cg constant **)
  | Constg of int32 (** g constant **)
  | ConstKL of int32 (** Kl constant**)
  | ConstML of int32 (** Ml constant **)
  | ConstCL of int32 (** Cl constant **)
  | ConstL of int32 (** l constant **)
  | ConstH of int32 (** h constant **)
  | ConstMin of int32 (** min constant **)
  | ConstS of int32 (** s constant **)

and binop = LMJ.binop =
  | OpAdd
  | OpSub
  | OpPower
  | OpMul
  | OpLt
  | OpGt
  | OpEquals
  | OpAnd
  | OpOr
  

and unop = LMJ.unop = 
  |UOpNot
  |UopIncr

and instruction =
  | IBlock of instruction list
  | IIf of expression * instruction * instruction
  | IWhile of expression * instruction
  | ISyso of expression
  | ISetVar of identifier * typ * expression
  | IArraySet of identifier * expression * expression

and typ =
  | TypInt
  | TypBool
  | TypIntArray
  | TypIntKm
  | TypIntMm
  | TypIntCm
  | TypIntm
  | TypIntKg
  | TypIntMg
  | TypIntCg
  | TypIntg
  | TypIntKL
  | TypIntML
  | TypIntCL
  | TypIntL
  | TypIntH
  | TypIntMin
  | TypIntS
  | Typ of identifier

and metho = {
    formals: (identifier * typ) list;
    result: typ;
    locals: (identifier * typ) list;
    body: instruction list;
    return: expression
  }

and clas = {
    extends: identifier option;
    attributes: (identifier * typ) list;
    methods: (identifier * metho) list
  }

and program = {
    name: identifier;
    defs: (identifier * clas) list;
    main_args: identifier;
    main: instruction
  }

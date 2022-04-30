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
  | ConstBool of bool
  | ConstInt of int32
  | ConstKm of int32
  | ConstMm of int32
  | ConstCm of int32
  | Constm of int32
  | ConstDam of int32
  | ConstDm of int32
  | ConstHm of int32
  | ConstKg of int32
  | ConstMg of int32
  | ConstCg of int32
  | Constg of int32
  | ConstDag of int32
  | ConstDg of int32
  | ConstHg of int32
  | ConstKl of int32
  | ConstMl of int32
  | ConstCl of int32
  | Constl of int32
  | ConstDal of int32
  | ConstDl of int32
  | ConstHl of int32
  | ConstMin of int32
  | ConstH of int32
  | ConstS of int32
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
  |UopDecr

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
  | TypIntHm
  | TypIntDam
  | TypIntDm
  | TypIntKl
  | TypIntMl
  | TypIntCl
  | TypIntl
  | TypIntHl
  | TypIntDal
  | TypIntDl
  | TypIntKg
  | TypIntMg
  | TypIntCg
  | TypIntg
  | TypIntHg
  | TypIntDag
  | TypIntDg
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

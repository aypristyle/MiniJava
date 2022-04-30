(** The abstract syntax tree for MiniJava with position informations
    to point the user to the location of the error in the source file
    when a syntax or typechecking error is encountered.
 *)

(** An identifier with position informations. *)
type identifier = string Location.t

(** An expression with position informations. *)
type expression = raw_expression Location.t

(** An expression without position informations. *)
and raw_expression =
  | EConst of constant (** A integer or boolean constant. *)
  | EGetVar of identifier (** Get the value of a variable. *)
  | EUnOp of unop * expression (** An unary operator. *)
  | EBinOp of binop * expression * expression (** [EBinOp (op, e1, e2)] represents the expression [e1 op e2]. *)
  | EMethodCall of expression * identifier * expression list (** [EMethodCall (o, id, [p1, ..., pn])] represents the call [o.id(p1, ..., pn)]. *)
  | EArrayGet of expression * expression (** [EArrayGet (e1, e2)] represents the expression [e1[e2]]. *)
  | EArrayAlloc of expression (** [EArrayAlloc e] represents the expression [new int[e]]. *)
  | EArrayLength of expression (** [EArrayLength e] represents the expression [e.length]. *)
  | EThis (** [EThis] represents the expression [this]. *)
  | EObjectAlloc of identifier (** [EObjectAlloc id] represents the expression [new id()]. *)

and constant =
  | ConstBool of bool (** Boolean constant [true] or [false]. *)
  | ConstInt of int32 (** Integer constant [[-2^31, 2^31 - 1]]. *)
  (* km hm dam m dm cm mm *)
  | ConstKm of int32	(** Km constant**)
  | ConstHm of int32	(** Hm constant**)
  | ConstDam of int32	(** Dam constant**)
  | Constm of int32	(** m constant**)
  | ConstDm of int32	(** Dm constant**)
  | ConstCm of int32	(** Cm constant**)
  | ConstMm of int32	(** Mm constant**)
  (* kg hg dag g dg cg mg *)
  | ConstKg of int32	(** Kg constant**)
  | ConstHg of int32	(** Hg constant**)
  | ConstDag of int32	(** Dag constant**)
  | Constg of int32	(** g constant**)
  | ConstDg of int32	(** Dg constant**)
  | ConstCg of int32	(** Cg constant**)
  | ConstMg of int32	(** Mg constant**)
  (* kl hl dal l dl cl ml *)
  | ConstKl of int32	(** Kl constant**)
  | ConstHl of int32	(** Hl constant**)
  | ConstDal of int32	(** Dal constant**)
  | Constl of int32	(** l constant**)
  | ConstDl of int32	(** Dl constant**)
  | ConstCl of int32	(** Cl constant**)
  | ConstMl of int32	(** Ml constant**)
   (* h min s *)
  | ConstH of int32	(** Heures constant**)
  | ConstMin of int32	(** Minutes constant**)
  | ConstS of int32	(** Secondes constant**)

and binop =
  | OpAdd  (** Binary operator [+]. *)
  | OpSub  (** Binary operator [-]. *)
  |OpPower (** Power operator [**]. *) 
  | OpMul  (** Binary operator [*]. *)
  | OpLt   (** Binary operator [<]. *)
  | OpGt   (** Binary operator [>]. *)
  | OpEquals (** Binary operator [==]. *)
  | OpAnd  (** Binary operator [&&]. *)
  | OpOr   (** Binary operator [||]. *)
  | OpDiv   (** Binary operator [||]. *)
  

and unop = 
   |UOpNot   (** Unary operator [!]. *)
   |UopIncr  (** Incremetal operator [++]. *)
   |UopDecr  (** Incremetal operator [--]. *)

and instruction =
  | IBlock of instruction list (** [IBlock [i1; i2; ...; in]] represents the instruction [{ i1 i2 ... in }]. *)
  | IIf of expression * instruction * instruction (** [IIf (e, i1, i2)] represents the instruction [if (e) i1 else i2]. *)
  | IWhile of expression * instruction (** [IWile (e, ins)] represents the instruction [while (e) ins]. *)
  | ISyso of expression (** [ISyso e] represents the instruction [System.out.println(e);]. *)
  | ISetVar of identifier * expression (** [ISetVar (id, e)] represents the instruction [id = e;]. *)
  | IArraySet of identifier * expression * expression (** [IArraySet (id, e1, e2)] represents the instruction [id[e1] = e2;]. *)

and typ =
  | TypInt (** Type [int]. *)
  | TypBool (** Type [bool]. *)
  | TypIntArray (** Type [int[]]. *)  
   (* km hm dam m dm cm mm *)
  | TypIntKm	(** Type integer of km *)
  | TypIntHm	(** Type integer of hm *)
  | TypIntDam	(** Type integer of dam *)
  | TypIntm	(** Type integer of m *)
  | TypIntDm	(** Type integer of dm *)
  | TypIntCm	(** Type integer of cm *)
  | TypIntMm	(** Type integer of mm *)
 (* kg hg dag g dg cg mg *)
  | TypIntKg	(** Type integer of kg *)
  | TypIntHg	(** Type integer of hg *)
  | TypIntDag	(** Type integer of dag *)
  | TypIntg	(** Type integer of g *)
  | TypIntDg	(** Type integer of dg *)
  | TypIntCg	(** Type integer of cg *)
  | TypIntMg	(** Type integer of mg *)
  (* kl hl dal l dl cl ml *)
  | TypIntKl	(** Type integer of kl *)
  | TypIntHl	(** Type integer of hl *)
  | TypIntDal	(** Type integer of dal *)
  | TypIntl	(** Type integer of l *)
  | TypIntDl	(** Type integer of dl *)
  | TypIntCl	(** Type integer of cl *)
  | TypIntMl	(** Type integer of ml *)
  (* h min s *)
  | TypIntH	(** Type integer of h *)
  | TypIntMin	(** Type integer of min *)
  | TypIntS	(** Type integer of s *)
  | Typ of identifier (** A class type. *)

and metho = {
  formals: (identifier * typ) list; (** The names of the parameters of the method with their types. *)
  result: typ; (** Result type of the method. *)
  locals: (identifier * typ) list; (** The names of the local variables with their types (declared at the beginning of the method). *)
  body: instruction list; (** The list of instructions of the method. *)
  return: expression (** The return expression. *)
}

and clas = {
  extends: identifier option; (** The parent class if any. *)
  attributes: (identifier * typ) list; (** The names of the attributes of the class with their types. *)
  methods: (identifier * metho) list (** The names of the methods of the class with their types. *)
}

and program = {
  name: identifier; (** The name of the main class. *)
  defs: (identifier * clas) list; (** The names and definitions of the other classes. *)
  main_args: identifier; (** The name of the parameter of the main method in the main class. *)
  main: instruction (** In MiniJava the main has only one instruction (but you can use
                        a block if you want more than one). *)
}

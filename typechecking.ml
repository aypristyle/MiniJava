open LMJ
open Printf

module SM = StringMap
module S = StringSet

type method_type = typ list * typ (** Parameters types and return type of a method. *)

type method_env = method_type SM.t

type attribute_env = typ SM.t

type class_type = attribute_env * method_env

type class_env = class_type SM.t

type variable_env = typ SM.t

exception Error of string

(** [error loc msg] raises an exception [Error] with the message [msg] and the
    position informations associated with [loc]. *)
let error (location : 'a Location.t) (msg : string) =
  raise (Error (sprintf "%s:\n%s"
                  (Error.positions (Location.startpos location) (Location.endpos location))
                  msg))

(** [error locs msg] raises an exception [Error] with the message [msg] and all
    the position informations of the list [locs]. *)
let errors (locations : 'a Location.t list) (msg : string) =
  raise (Error (sprintf "%s%s"
                  (List.fold_right (fun location acc ->
                      sprintf "%s:\n%s" (Error.positions (Location.startpos location) (Location.endpos location)) acc
                   ) locations "") msg))

(** [lookup msg id env] lookups the identifier [id] in the map [env].
    If the identifier is not present raises an error using the message [msg]. *)
let lookup (msg : string) (id : identifier) (env : 'a SM.t) =
  try
    SM.find (Location.content id) env
  with Not_found ->
    error id (sprintf "%s %s is undefined" msg (Location.content id))

(** [vlookup id env] lookups the variable [id] in the environment for variables (locals or parameters) [env]. *)
let vlookup : identifier -> variable_env -> typ = lookup "variable"

(** [mlookup m env] lookups the method [m] in the environment for methods [env]. *)
let mlookup : identifier -> method_env -> method_type = lookup "method"

(** [alookup a env] lookups the attribute [a] in the environment for attributes [env]. *)
let alookup : identifier -> attribute_env -> typ = lookup "attribute"

(** [clookup c env] lookups the class [c] in the environment for classes [env]. *)
let clookup : identifier -> class_env -> class_type = lookup "class"

(** [compatible t1 t2 instanceof] returns true iff the type [t1] is compatible with type [t2].
    For classes, uses the function [instanceof] to decide if [t1] is an instance of [t2]. *)
let rec compatible (typ1 : typ) (typ2 : typ) (instanceof : identifier -> identifier -> bool) : bool =
  match typ1, typ2 with
  | TypInt, TypInt
  | TypBool, TypBool
  (* km hm dam m dm cm mm *)
  | TypIntKm, TypIntKm
  | TypIntHm, TypIntHm
  | TypIntDam, TypIntDam
  | TypIntm, TypIntm
  | TypIntDm, TypIntDm
  | TypIntCm, TypIntCm
  | TypIntMm, TypIntMm
 
  | TypIntKm, TypIntHm
  | TypIntKm, TypIntDam
  | TypIntKm, TypIntm
  | TypIntKm, TypIntDm
  | TypIntKm, TypIntCm
  | TypIntKm, TypIntMm
  
  | TypIntHm, TypIntDam
  | TypIntHm, TypIntm
  | TypIntHm, TypIntDm
  | TypIntHm, TypIntCm
  | TypIntHm, TypIntMm
  
  | TypIntDam, TypIntm
  | TypIntDam, TypIntDm
  | TypIntDam, TypIntCm
  | TypIntDam, TypIntMm
  
  | TypIntm, TypIntDm
  | TypIntm, TypIntCm
  | TypIntm, TypIntMm
  
  | TypIntDm, TypIntCm
  | TypIntDm, TypIntMm
  
  | TypIntCm, TypIntMm
  
  (* kg hg dag g dg cg mg *)
  | TypIntKg, TypIntKg
  | TypIntHg, TypIntHg
  | TypIntDag, TypIntDag
  | TypIntg, TypIntg
  | TypIntDg, TypIntDg
  | TypIntCg, TypIntCg
  | TypIntMg, TypIntMg
  
  | TypIntKg, TypIntHg
  | TypIntKg, TypIntDag
  | TypIntKg, TypIntg
  | TypIntKg, TypIntDg
  | TypIntKg, TypIntCg
  | TypIntKg, TypIntMg
  
  | TypIntHg, TypIntDag
  | TypIntHg, TypIntg
  | TypIntHg, TypIntDg
  | TypIntHg, TypIntCg
  | TypIntHg, TypIntMg
  
  | TypIntDag, TypIntg
  | TypIntDag, TypIntDg
  | TypIntDag, TypIntCg
  | TypIntDag, TypIntMg
  
  | TypIntg, TypIntDg
  | TypIntg, TypIntCg
  | TypIntg, TypIntMg
  
  | TypIntDg, TypIntCg
  | TypIntDg, TypIntMg
  
  | TypIntCg, TypIntMg
  (* kl hl dal l dl cl ml *)
  | TypIntKl, TypIntKl
  | TypIntHl, TypIntHl
  | TypIntDal, TypIntDal
  | TypIntl, TypIntl
  | TypIntDl, TypIntDl
  | TypIntCl, TypIntCl
  | TypIntMl, TypIntMl
  
  | TypIntKl, TypIntHl
  | TypIntKl, TypIntDal
  | TypIntKl, TypIntl
  | TypIntKl, TypIntDl
  | TypIntKl, TypIntCl
  | TypIntKl, TypIntMl
  
  | TypIntHl, TypIntDal
  | TypIntHl, TypIntl
  | TypIntHl, TypIntDl
  | TypIntHl, TypIntCl
  | TypIntHl, TypIntMl
  
  | TypIntDal, TypIntl
  | TypIntDal, TypIntDl
  | TypIntDal, TypIntCl
  | TypIntDal, TypIntMl
  
  | TypIntl, TypIntDl
  | TypIntl, TypIntCl
  | TypIntl, TypIntMl
  
  | TypIntDl, TypIntCl
  | TypIntDl, TypIntMl
  
  | TypIntCl, TypIntMl
  (* h min s *)
  | TypIntH,TypIntH
  | TypIntMin,TypIntMin
  | TypIntS,TypIntS
  | TypIntH, TypIntMin
  | TypIntH, TypIntS
  | TypIntS, TypIntMin
  | TypIntArray, TypIntArray -> true
  | Typ t1, Typ t2 -> instanceof t1 t2
  | _, _ -> false


(** [typ_lmj_to_tmj t] converts the [LMJ] type [t] into the equivalent [TMJ] type. *)
let rec type_lmj_to_tmj = function
  | TypInt      -> TMJ.TypInt
  | TypBool     -> TMJ.TypBool
   (* km hm dam m dm cm mm *)
  | TypIntKm     -> TMJ.TypIntKm
  | TypIntHm    -> TMJ.TypIntHm
  | TypIntDam    -> TMJ.TypIntDam
  | TypIntm    -> TMJ.TypIntm
  | TypIntDm    -> TMJ.TypIntDm
  | TypIntCm    -> TMJ.TypIntCm
  | TypIntMm    -> TMJ.TypIntMm
  (* kg hg dag g dg cg mg *)
  | TypIntKg     -> TMJ.TypIntKg
  | TypIntHg    -> TMJ.TypIntHg
  | TypIntDag    -> TMJ.TypIntDag
  | TypIntg    -> TMJ.TypIntg
  | TypIntDg    -> TMJ.TypIntDg
  | TypIntCg    -> TMJ.TypIntCg
  | TypIntMg    -> TMJ.TypIntMg
  (* kl hl dal l dl cl ml *)
  | TypIntKl     -> TMJ.TypIntKl
  | TypIntHl    -> TMJ.TypIntHl
  | TypIntDal    -> TMJ.TypIntDal
  | TypIntl    -> TMJ.TypIntl
  | TypIntDl    -> TMJ.TypIntDl
  | TypIntCl    -> TMJ.TypIntCl
  | TypIntMl    -> TMJ.TypIntMl
  (* h min s *)
  | TypIntH    -> TMJ.TypIntH
  | TypIntMin    -> TMJ.TypIntMin
  | TypIntS     -> TMJ.TypIntS
  | TypIntArray -> TMJ.TypIntArray
  | Typ id      -> TMJ.Typ (Location.content id)

(** [typ_tmj_to_lmj s e t] converts the [TMJ] type [t] into the equivalent [LMJ] type using location starting position [s] and location ending position [e]. *)
let rec type_tmj_to_lmj startpos endpos = function
| TMJ.TypInt      -> TypInt
 (* km hm dam m dm cm mm *)
|TMJ.TypIntKm     -> TypIntKm
|TMJ.TypIntHm     -> TypIntHm
|TMJ.TypIntDam     -> TypIntDam
|TMJ.TypIntm     -> TypIntm
|TMJ.TypIntDm     ->TypIntDm
|TMJ.TypIntCm     ->TypIntCm
|TMJ.TypIntMm     ->TypIntMm
(* kg hg dag g dg cg mg *)
|TMJ.TypIntKg     -> TypIntKg
|TMJ.TypIntHg     -> TypIntHg
|TMJ.TypIntDag     -> TypIntDag
|TMJ.TypIntg     -> TypIntg
|TMJ.TypIntDg     ->TypIntDg
|TMJ.TypIntCg     ->TypIntCg
|TMJ.TypIntMg     ->TypIntMg
(* kl hl dal l dl cl ml *)
|TMJ.TypIntKl     -> TypIntKl
|TMJ.TypIntHl     -> TypIntHl
|TMJ.TypIntDal     -> TypIntDal
|TMJ.TypIntl     -> TypIntl
|TMJ.TypIntDl     ->TypIntDl
|TMJ.TypIntCl     ->TypIntCl
|TMJ.TypIntMl     ->TypIntMl
(* h min s *)
|TMJ.TypIntH     ->TypIntH
|TMJ.TypIntMin     ->TypIntMin
|TMJ.TypIntS     ->TypIntS
| TMJ.TypBool     -> TypBool
| TMJ.TypIntArray -> TypIntArray
| TMJ.Typ id      -> Typ (Location.make startpos endpos id)

(** [tmj_type_to_string t] converts the [TMJ] type [t] into a string representation. *)
let rec tmj_type_to_string : TMJ.typ -> string = function
  | TMJ.TypInt -> "integer"
  | TMJ.TypBool -> "boolean"
  | TMJ.TypIntArray -> "int[]"
  (* km hm dam m dm cm mm *)
  | TMJ.TypIntKm -> "km"
  | TMJ.TypIntHm -> "hm"
  | TMJ.TypIntDam -> "dam"
  | TMJ.TypIntm -> "m"
  | TMJ.TypIntDm -> "dm"
  | TMJ.TypIntCm -> "cm"
  | TMJ.TypIntMm -> "mm"
  (* kg hg dag g dg cg mg *)
  | TMJ.TypIntKg -> "kg"
  | TMJ.TypIntHg -> "hg"
  | TMJ.TypIntDag -> "dag"
  | TMJ.TypIntg -> "g"
  | TMJ.TypIntDg -> "dg"
  | TMJ.TypIntCg -> "cg"
  | TMJ.TypIntMg -> "mg"
  (* kl hl dal l dl cl ml *)
  | TMJ.TypIntKl -> "kl"
  | TMJ.TypIntHl -> "hl"
  | TMJ.TypIntDal -> "dal"
  | TMJ.TypIntl -> "l"
  | TMJ.TypIntDl -> "dl"
  | TMJ.TypIntCl -> "cl"
  | TMJ.TypIntMl -> "ml"
  (* h min s *)
  | TMJ.TypIntH -> "h"
  | TMJ.TypIntMin -> "min"
  | TMJ.TypIntS -> "s"
  | TMJ.Typ t -> t

(** [type_to_string t] converts the [LMJ] type [t] into a string representation. *)
let rec type_to_string (typ : LMJ.typ) : string =
  type_lmj_to_tmj typ
  |> tmj_type_to_string

(** [mke r t] creates a [TMJ] expression with raw expression [r] and type [t]. *)
let mke raw_expression typ = TMJ.{ raw_expression; typ = type_lmj_to_tmj typ }

(** [typecheck_call cenv venv vinit instanceof o callee es] checks, using the environments [cenv] and [venv],
    the set of initialized variables [vinit] and the [instanceof] function, that
     * the expression [o] is an object of type [t],
     * the method [callee] belongs to the class [t],
     * the parameters [es] are compatibles with the types of the formal parameters.
    If [typecheck_call] succeeds, the return type of [callee] is returned. *)
let rec typecheck_call (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (o : expression)
    (callee : identifier)
    (expressions : expression list) : TMJ.expression =
  let o' = typecheck_expression cenv venv vinit instanceof o in
  match o'.typ with
  | Typ t ->
    begin
      let _, method_env = Location.(clookup (make (startpos o) (endpos o) t) cenv) in
      let (formals : typ list), (result : typ) = mlookup callee method_env in
      try
        let expressions' =
          List.fold_left2 (fun acc formal e -> typecheck_expression_expecting cenv venv vinit instanceof formal e :: acc) [] formals expressions
          |> List.rev 
        in
        mke (TMJ.EMethodCall (o', Location.content callee, expressions')) result
      with Invalid_argument _ ->
        error callee
          (sprintf "Invalid function call, expected %d arguments, got %d"
             (List.length formals)
             (List.length expressions))
    end
  | _ -> error o (sprintf "A class is expected, got %s" (tmj_type_to_string o'.typ))


(** [typecheck_expression_expecting cenv venv vinit instanceof typ1 e] checks, using the
    environments [cenv] and [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] has a type compatible with type [typ1]. *)
and typecheck_expression_expecting (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (typ1 : typ)
    (e : expression) : TMJ.expression =
  let e' = typecheck_expression cenv venv vinit instanceof e in
  if not (compatible Location.(type_tmj_to_lmj (startpos e) (endpos e) e'.typ) typ1 instanceof) then
    error e
      (sprintf "Type mismatch, expected %s, got %s" (type_to_string typ1) (tmj_type_to_string e'.typ));
  e'

(** [typecheck_expression cenv venv vinit instanceof e] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] is well typed.
    If [typecheck_expression] succeeds, the type of [e] is returned. *)
and typecheck_expression (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (e : expression) : TMJ.expression =
  match Location.content e with
  | EConst (ConstBool b) -> 
      mke (TMJ.EConst (ConstBool b)) TypBool
  | EConst (ConstInt i) ->
      mke (TMJ.EConst (ConstInt i)) TypInt
   (* km hm dam m dm cm mm *)
   | EConst (ConstKm i) ->
      mke (TMJ.EConst (ConstKm i)) TypIntKm
   | EConst (ConstHm i) ->
      mke (TMJ.EConst (ConstHm i)) TypIntHm
   | EConst (ConstDam i) ->
      mke (TMJ.EConst (ConstDam i)) TypIntDam
   | EConst (Constm i) ->
      mke (TMJ.EConst (Constm i)) TypIntm
   | EConst (ConstDm i) ->
      mke (TMJ.EConst (ConstDm i)) TypIntDm
   | EConst (ConstCm i) ->
      mke (TMJ.EConst (ConstCm i)) TypIntCm
   | EConst (ConstMm i) ->
      mke (TMJ.EConst (ConstMm i)) TypIntMm
   (* kg hg dag g dg cg mg *)
   | EConst (ConstKg i) ->
      mke (TMJ.EConst (ConstKg i)) TypIntKg
   | EConst (ConstHg i) ->
      mke (TMJ.EConst (ConstHg i)) TypIntHg
   | EConst (ConstDag i) ->
      mke (TMJ.EConst (ConstDag i)) TypIntDag
   | EConst (Constg i) ->
      mke (TMJ.EConst (Constg i)) TypIntg
   | EConst (ConstDg i) ->
      mke (TMJ.EConst (ConstDg i)) TypIntDg
   | EConst (ConstCg i) ->
      mke (TMJ.EConst (ConstCg i)) TypIntCg
   | EConst (ConstMg i) ->
      mke (TMJ.EConst (ConstMg i)) TypIntMg
   (* kl hl dal l dl cl ml *)
  | EConst (ConstKl i) ->
      mke (TMJ.EConst (ConstKl i)) TypIntKl
   | EConst (ConstHl i) ->
      mke (TMJ.EConst (ConstHl i)) TypIntHl
   | EConst (ConstDal i) ->
      mke (TMJ.EConst (ConstDal i)) TypIntDal
   | EConst (Constl i) ->
      mke (TMJ.EConst (Constl i)) TypIntl
   | EConst (ConstDl i) ->
      mke (TMJ.EConst (ConstDl i)) TypIntDl
   | EConst (ConstCl i) ->
      mke (TMJ.EConst (ConstCl i)) TypIntCl
   | EConst (ConstMl i) ->
      mke (TMJ.EConst (ConstMl i)) TypIntMl
   (* h min s *)
  | EConst (ConstH i) ->
      mke (TMJ.EConst (ConstH i)) TypIntH
   | EConst (ConstMin i) ->
      mke (TMJ.EConst (ConstMin i)) TypIntMin
   | EConst (ConstS i) ->
      mke (TMJ.EConst (ConstS i)) TypIntS




  | EGetVar v ->
     let typ = vlookup v venv in
     let v' = Location.content v in
     if not (S.mem v' vinit) then
       error v (sprintf "Variable %s has not been initialized" v');
     mke (TMJ.EGetVar (Location.content v)) typ

  | EUnOp (op, e) ->
      let expected, returned =
        match op with
        | UOpNot -> TypBool, TypBool
        | UopIncr -> TypInt, TypInt
      in
      let e' = typecheck_expression_expecting cenv venv vinit instanceof expected e in
      mke (TMJ.EUnOp (op, e')) returned

   | EBinOp (OpEquals, e1, e2) -> 
  	let e1' = typecheck_expression cenv venv vinit instanceof  e1 in 
  	let e2' = typecheck_expression cenv venv vinit instanceof  e2 in
  	let t1 = type_tmj_to_lmj(Location.startpos e1) (Location.endpos e1) e1'.typ in
  	let t2 = type_tmj_to_lmj(Location.startpos e2) (Location.endpos e2) e2'.typ in
  	if compatible t1 t2 instanceof || compatible t2 t1 instanceof then mke (TMJ.EBinOp(OpEquals, e1', e2')) TypBool else error e "The two expressions have different types"
  	
  | EBinOp ((OpAdd), e1, e2) -> 
  	let e1' = typecheck_expression cenv venv vinit instanceof  e1 in 
  	let e2' = typecheck_expression cenv venv vinit instanceof  e2 in
  	let t = match e1'.typ, e2'.typ with
  	(*on vérifie les types des paramètres en entrée pour traiter le cas où on aurait une addition de km et m => on veut retourner des m*)
  		(* KM HM DAM M DM CM MM *)
  		| TypIntKm,TypIntKm -> TypIntKm
  		| TypIntKm,TypIntHm | TypIntHm,TypIntKm -> TypIntHm
  		| TypIntKm,TypIntDam | TypIntDam,TypIntKm | TypIntHm,TypIntDam | TypIntDam,TypIntHm -> TypIntDam
  		| TypIntKm,TypIntm | TypIntm,TypIntKm | TypIntHm,TypIntm | TypIntm,TypIntHm | TypIntDam,TypIntm | TypIntm,TypIntDam-> TypIntm
  		| TypIntKm,TypIntDm | TypIntDm,TypIntKm | TypIntHm,TypIntDm | TypIntDm,TypIntHm | TypIntDam,TypIntDm | TypIntDm,TypIntDam | TypIntm,TypIntDm | TypIntDm,TypIntm-> TypIntDm
  		| TypIntKm,TypIntDm | TypIntDm,TypIntKm | TypIntHm,TypIntDm | TypIntDm,TypIntHm | TypIntDam,TypIntDm | TypIntDm,TypIntDam | TypIntm,TypIntDm | TypIntDm,TypIntm-> TypIntCm
  		| TypIntKm,TypIntCm | TypIntCm,TypIntKm | TypIntHm,TypIntCm | TypIntCm,TypIntHm | TypIntDam,TypIntCm | TypIntCm,TypIntDam | TypIntm,TypIntCm | TypIntCm,TypIntm | TypIntDm,TypIntCm | TypIntCm,TypIntDm-> TypIntCm
  		| TypIntKm,TypIntMm | TypIntMm,TypIntKm | TypIntHm,TypIntMm | TypIntMm,TypIntHm | TypIntDam,TypIntMm | TypIntMm,TypIntDam | TypIntm,TypIntMm | TypIntMm,TypIntm | TypIntDm,TypIntMm | TypIntMm,TypIntDm | TypIntCm,TypIntMm | TypIntMm,TypIntCm  -> TypIntMm
  		
  		(*
  		| TypIntMm,TypIntCm | TypIntCm,TypIntMm -> TypIntMm
  		| _,TypIntCm | TypIntCm,_ -> TypIntCm
  		| _,TypIntMm | TypIntMm,_ -> TypIntMm
  		| TypIntKm,TypIntKm  -> TypIntKm 
  		*)
  		(* KG HG DAG G DG CG MG *)
  		| TypIntKg,TypIntg | TypIntg,TypIntKg -> TypIntg
  		| _,TypIntCg | TypIntCg,_ -> TypIntCg
  		| _,TypIntMg | TypIntMg,_ -> TypIntMg
  		| TypIntKg,TypIntKg  -> TypIntKg
  		(* KL HL DAL L DL CL ML *)
  		| TypIntKl,TypIntl | TypIntl,TypIntKl -> TypIntl
  		| TypIntMl,TypIntCl | TypIntCl,TypIntMl -> TypIntMl
  		| _,TypIntCl | TypIntCl,_ -> TypIntCl
  		| _,TypIntMl | TypIntMl,_ -> TypIntMl
  		| TypIntKl,TypIntKl  -> TypIntKl 
  		(* H Min S *)
  		| TypIntH,TypIntH -> TypIntH
  		| TypIntH,TypIntS | TypIntS,TypIntH | TypIntS,TypIntS -> TypIntS
  		| _,TypIntMin | TypIntMin,_ -> TypIntMin
  		| TypInt, TypInt -> TypInt 
  		| _ -> error e "The two expressions have uncompatible types, hence no addition possible" in
  		mke (TMJ.EBinOp(OpAdd, e1', e2')) t 
  	
  | EBinOp ((OpSub), e1, e2) -> 
  	let e1' = typecheck_expression cenv venv vinit instanceof  e1 in 
  	let e2' = typecheck_expression cenv venv vinit instanceof  e2 in
  	let t = match e1'.typ, e2'.typ with
  	(*on vérifie les types des paramètres en entrée pour traiter le cas où on aurait une addition de km et m => on veut retourner des m*)
  		(* KM HM DAM M DM CM MM *)
  		| TypIntKm,TypIntm | TypIntm,TypIntKm -> TypIntm
  		| TypIntMm,TypIntCm | TypIntCm,TypIntMm -> TypIntMm
  		| _,TypIntCm | TypIntCm,_ -> TypIntCm
  		| _,TypIntMm | TypIntMm,_ -> TypIntMm
  		| TypIntKm,TypIntKm  -> TypIntKm 
  		(* KG HG DAG G DG CG MG *)
  		| TypIntKg,TypIntg | TypIntg,TypIntKg -> TypIntg
  		| _,TypIntCg | TypIntCg,_ -> TypIntCg
  		| _,TypIntMg | TypIntMg,_ -> TypIntMg
  		| TypIntKg,TypIntKg  -> TypIntKg
  		(* KL HL DAL L DL CL ML *)
  		| TypIntKl,TypIntl | TypIntl,TypIntKl -> TypIntl
  		| TypIntMl,TypIntCl | TypIntCl,TypIntMl -> TypIntMl
  		| _,TypIntCl | TypIntCl,_ -> TypIntCl
  		| _,TypIntMl | TypIntMl,_ -> TypIntMl
  		| TypIntKl,TypIntKl  -> TypIntKl  
  		(* H Min S *)
  		| TypIntH,TypIntH -> TypIntH
  		| TypIntH,TypIntS | TypIntS,TypIntH | TypIntS,TypIntS -> TypIntS
  		| _,TypIntMin | TypIntMin,_ -> TypIntMin
  		| TypInt, TypInt -> TypInt  
  		| _ -> error e "The two expressions have uncompatible types, hence no substraction possible" in
  		mke (TMJ.EBinOp(OpSub, e1', e2')) t 
 
 | EBinOp ((OpMul), e1, e2) -> 
  	let e1' = typecheck_expression cenv venv vinit instanceof  e1 in 
  	let e2' = typecheck_expression cenv venv vinit instanceof  e2 in
  	let t = match e1'.typ, e2'.typ with
  	(*on vérifie les types des paramètres en entrée pour traiter le cas où on aurait une addition de km et m => on veut retourner des m*)
  		(* KM HM DAM M DM CM MM *)
  		| TypIntKm,TypIntm | TypIntm,TypIntKm -> TypIntm
  		| TypIntMm,TypIntCm | TypIntCm,TypIntMm -> TypIntMm
  		| _,TypIntCm | TypIntCm,_ -> TypIntCm
  		| _,TypIntMm | TypIntMm,_ -> TypIntMm
  		| TypIntKm,TypIntKm  -> TypIntKm 
  		(* KG HG DAG G DG CG MG *)
  		| TypIntKg,TypIntg | TypIntg,TypIntKg -> TypIntg
  		| _,TypIntCg | TypIntCg,_ -> TypIntCg
  		| _,TypIntMg | TypIntMg,_ -> TypIntMg
  		| TypIntKg,TypIntKg  -> TypIntKg
  		(* KL HL DAL L DL CL ML *)
  		| TypIntKl,TypIntl | TypIntl,TypIntKl -> TypIntl
  		| TypIntMl,TypIntCl | TypIntCl,TypIntMl -> TypIntMl
  		| _,TypIntCl | TypIntCl,_ -> TypIntCl
  		| _,TypIntMl | TypIntMl,_ -> TypIntMl
  		| TypIntKl,TypIntKl  -> TypIntKl 
  		(* H Min S *)
  		| TypIntH,TypIntH -> TypIntH
  		| TypIntH,TypIntS | TypIntS,TypIntH | TypIntS,TypIntS -> TypIntS
  		| _,TypIntMin | TypIntMin,_ -> TypIntMin
  		| TypInt, TypInt -> TypInt 
  		| _ -> error e "The two expressions have uncompatible types, hence no multiplication possible" in
  		mke (TMJ.EBinOp(OpMul, e1', e2')) t 
  		
  | EBinOp (op, e1, e2) ->
      let expected, returned =
        match op with
        | OpAdd
        | OpSub
        | OpPower -> TypInt, TypInt
        | OpMul -> TypInt, TypInt
        | OpLt  -> TypInt, TypBool
        | OpGt  -> TypInt, TypBool
        | OpEquals -> assert false
        | OpAnd -> TypBool, TypBool
        | OpOr  -> TypBool, TypBool
      in
      let e1' = typecheck_expression_expecting cenv venv vinit instanceof expected e1 in
      let e2' = typecheck_expression_expecting cenv venv vinit instanceof expected e2 in
      mke (TMJ.EBinOp (op, e1', e2')) returned

  | EMethodCall (o, callee, expressions) ->
      typecheck_call cenv venv vinit instanceof o callee expressions

  | EArrayGet (earray, eindex) ->
      let eindex' = typecheck_expression_expecting cenv venv vinit instanceof TypInt eindex in
      let earray' = typecheck_expression_expecting cenv venv vinit instanceof TypIntArray earray in
      mke (TMJ.EArrayGet (earray', eindex')) TypInt

  | EArrayAlloc elength ->
      let elength' = typecheck_expression_expecting cenv venv vinit instanceof TypInt elength in
      mke (TMJ.EArrayAlloc elength') TypIntArray

  | EArrayLength earray ->
      let earray' = typecheck_expression_expecting cenv venv vinit instanceof TypIntArray earray in
      mke (TMJ.EArrayLength earray') TypInt

  | EThis ->
     mke TMJ.EThis (vlookup (Location.make (Location.startpos e) (Location.endpos e) "this") venv)

  | EObjectAlloc id ->
      clookup id cenv |> ignore;
      mke (TMJ.EObjectAlloc (Location.content id)) (Typ id)

(** [typecheck_instruction cenv venv vinit instanceof inst] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the instruction [inst] is well typed.
    If [typecheck_instruction] succeeds, the new set of initialized variables is returned. *)
let rec typecheck_instruction (cenv : class_env) (venv : variable_env) (vinit : S.t)
    (instanceof : identifier -> identifier -> bool)
    (inst : instruction) : (TMJ.instruction * S.t) =
  match inst with
  | ISetVar (v, e) ->
      let vinit =
        S.add (Location.content v) vinit
      in
      let typ = vlookup v venv in
      let e' = typecheck_expression_expecting cenv venv vinit instanceof typ e in
      (TMJ.ISetVar (Location.content v, type_lmj_to_tmj typ, e'), vinit)

  | IArraySet (earray, eindex, evalue) ->
      typecheck_expression_expecting cenv venv vinit instanceof TypIntArray
        (Location.make (Location.startpos earray) (Location.endpos earray) (EGetVar earray))
      |> ignore;
      let eindex' = typecheck_expression_expecting cenv venv vinit instanceof TypInt eindex in
      let evalue' = typecheck_expression_expecting cenv venv vinit instanceof TypInt evalue in
      (TMJ.IArraySet (Location.content earray, eindex', evalue'), vinit)

  | IBlock instructions ->
      let instructions', vinit =
        List.fold_left
          (fun (acc, vinit) inst ->
          let inst, vinit = typecheck_instruction cenv venv vinit instanceof inst in
          (inst :: acc, vinit))
        ([], vinit)
        instructions
      in
      (TMJ.IBlock (List.rev instructions'), vinit)

  | IIf (cond, ithen, ielse) ->
      let cond' = typecheck_expression_expecting cenv venv vinit instanceof TypBool cond in
      let ithen', vinit1 =
        typecheck_instruction cenv venv vinit instanceof ithen
      in
      let ielse', vinit2 =
        typecheck_instruction cenv venv vinit instanceof ielse
      in
      (TMJ.IIf (cond', ithen', ielse'), S.inter vinit1 vinit2)

  | IWhile (cond, ibody) ->
      let cond' = typecheck_expression_expecting cenv venv vinit instanceof TypBool cond in
      let ibody', vinit = typecheck_instruction cenv venv vinit instanceof ibody in
      (TMJ.IWhile (cond', ibody'), vinit)

 (** | ISyso e ->
     let e' = typecheck_expression_expecting cenv venv vinit instanceof TypInt e in
     (TMJ.ISyso e', vinit)
    **) 
     | ISyso e ->
     let e1 = typecheck_expression cenv venv vinit instanceof e in 
     if e1.typ= TMJ.TypInt || e1.typ=TMJ.TypIntKm || e1.typ=TMJ.TypIntHm || e1.typ=TMJ.TypIntDam|| e1.typ=TMJ.TypIntm || e1.typ=TMJ.TypIntDm ||e1.typ=TMJ.TypIntCm || e1.typ=TMJ.TypIntMm || e1.typ=TMJ.TypIntKg || e1.typ=TMJ.TypIntHg || e1.typ=TMJ.TypIntDag || e1.typ=TMJ.TypIntg || e1.typ=TMJ.TypIntDg|| e1.typ=TMJ.TypIntCg || e1.typ=TMJ.TypIntMg ||e1.typ=TMJ.TypIntKl || e1.typ=TMJ.TypIntHl || e1.typ=TMJ.TypIntDal || e1.typ=TMJ.TypIntl || e1.typ=TMJ.TypIntDl || e1.typ=TMJ.TypIntCl || e1.typ=TMJ.TypIntMl ||e1.typ=TMJ.TypIntH || e1.typ=TMJ.TypIntMin || e1.typ=TMJ.TypIntS then (TMJ.ISyso e1, vinit) else error e "Not good"

(** [occurences x bindings] returns the elements in [bindings] that have [x] has identifier. *)
let occurrences (x : string) (bindings : (identifier * 'a) list) : identifier list =
  List.map fst (List.filter (fun (id, _) -> x = Location.content id) bindings)

(** [map_of_association_list entity bindings] creates a map from the association list [bindings].
    If some identifiers are duplicated, [map_of_association_list] raises an [Error] exception,
    using the string [entity] in the error message. *)
let map_of_association_list (entity : string) (bindings : (identifier * 'a) list) : 'a SM.t =
  try
    SM.of_association_list (List.map (fun (id, data) -> (Location.content id, data)) bindings)
  with SM.Duplicate x ->
    errors (occurrences x bindings) (sprintf "%s %s is declared more than once" entity x)

(** [variable_map decls] creates an environment for variables using the association list [decls]. *)
let variable_map (decls : (identifier * typ) list) : variable_env =
  map_of_association_list "Variable" decls

(** [method_map decls] creates an environment for methods using the association list [decls]. *)
let method_map (decls : (identifier * method_type) list) : method_env =
  map_of_association_list "Method" decls

(** [typecheck_method cenv venv instanceof m] checks, using the environments [cenv] and [venv]
    and the [instanceof] function, that the method [m] is well typed. *)
let typecheck_method (cenv : class_env) (venv : variable_env)
    (instanceof : identifier -> identifier -> bool)
    (m : metho) : TMJ.metho =

  let formals = m.formals
  and locals = m.locals in

  let mformals = variable_map formals
  and mlocals = variable_map locals in

  begin
    try
      let x =
        StringSet.choose
          (StringSet.inter
             (SM.domain mformals)
             (SM.domain mlocals))
      in
      errors (occurrences x formals @ occurrences x locals)
        "A formal parameter and a local variable cannot carry the same name"
    with Not_found ->
      ()
  end;

  let venv =
    SM.addm mformals venv
  |> SM.addm mlocals
  in

  let vinit =
    S.diff (SM.domain venv) (SM.domain mlocals)
  in
  let body', vinit =
    match typecheck_instruction cenv venv vinit instanceof (IBlock m.body) with 
    | IBlock body', vinit -> body', vinit 
    | _ -> assert false
  in
  let return' = typecheck_expression_expecting cenv venv vinit instanceof m.result m.return in
  TMJ.{
    formals = List.map (fun (id, typ) -> Location.content id, type_lmj_to_tmj typ) m.formals;
    result  = type_lmj_to_tmj m.result;
    locals  = List.map (fun (id, typ) -> Location.content id, type_lmj_to_tmj typ) m.locals;
    body    = body';
    return  = return'
  }

(** [typecheck_class cenv instanceof (name, c)] checks, using the environments [cenv] and [venv]
    and the [instanceof] function, that the class [name] with type [c] is well typed. *)
let typecheck_class (cenv : class_env) (instanceof : identifier -> identifier -> bool)
    ((name, c) : identifier * clas) : TMJ.identifier * TMJ.clas =
  let attribute_env, _ = clookup name cenv in
  let venv = SM.add "this" (Typ name) attribute_env in
  let methods' = 
    List.map (fun (id, metho) -> 
                (Location.content id, typecheck_method cenv venv instanceof metho)
              ) c.methods
  in
  (Location.content name, 
  TMJ.{
    extends    = (match c.extends with None -> None | Some id -> Some (Location.content id));
    attributes = List.map (fun (id, typ) -> Location.content id, type_lmj_to_tmj typ) c.attributes;
    methods    = methods';
  })

(** [extract_method_type m] creates a [method_type] from the method [m]. *)
let extract_method_type (m : metho) : method_type =
  (List.map snd m.formals, m.result)

(** [extract_class_type c] creates a [class_type] from the class [c]. *)
let extract_class_type (c : clas) : class_type =
  (variable_map c.attributes,
   method_map (List.map (fun (id, m) -> (id, extract_method_type m)) c.methods))

(** [class_map decls] creates an environment for classes using the association list [decls]. *)
let class_map (decls : (identifier * clas) list) : clas SM.t =
  map_of_association_list "Class" decls

(** [create_instancef cmap] creates an [instanceof] function such that
    [instanceof id1 id2] is true iff class [id2] is a parent (direct or indirect)
    of class [id1]. *)
let create_instanceof (cmap : clas SM.t) : identifier -> identifier -> bool =
  let rec instanceof id1 id2 =
    if id1 = id2 then true
    else
      try
        match (SM.find id1 cmap).extends with
        | None -> false
        | Some id3 -> instanceof (Location.content id3) id2
      with Not_found -> false
  in
  fun id1 id2 ->
    instanceof (Location.content id1) (Location.content id2)
  (* let memo = Hashtbl.create 97 in *)
  (* fun id1 id2 -> *)
  (*   let id1', id2' = Location.content id1, Location.content id2 in *)
  (*   try *)
  (*     Hashtbl.find memo (id1', id2') *)
  (*   with Not_found -> *)
  (*     let res = instanceof id1' id2' in *)
  (*     Hashtbl.add memo (id1', id2') res; *)
  (*     res *)

(** [add_method cmap instanceof] completes each class in [cmap] by creating a new map where we add
    to a given class the methods and attributes of its parents. If a method in a parent class has
    the same name than a method in a subclass, we check that the later overrides the former. *)
let add_method
      (cmap : clas SM.t)
      (instanceof : identifier -> identifier -> bool)
    : clas SM.t =
  let test_compatible_signature ((name, m) : identifier * metho) ((name', m') : identifier * metho) : unit =
    let typecheck_params (typ : typ) (typ' : typ) : unit =
      if not (compatible typ typ'
                (fun t1 t2 -> Location.content t1 = Location.content t2))
      then
        errors [name; name']
          (sprintf "Type mismatch in params of overriden method, expected %s, got %s" (type_to_string typ) (type_to_string typ'))
    in
    let typecheck_result (typ : typ) (typ' : typ) : unit =
      if not (compatible typ' typ instanceof) then
          errors [name; name']
            (sprintf "Type mismatch in result of overriden method, expected %s, got %s" (type_to_string typ) (type_to_string typ'))
    in
    let formals, result = extract_method_type m
    and formals', result' = extract_method_type m' in
    try
      List.iter2 typecheck_params formals formals';
      typecheck_result result result'
    with Invalid_argument _ ->
      errors [name; name']
        (sprintf "A function that overrides another one must have the same number of parameters" )
  in
  (**
    [complete o c] adds to the class [c] all methods and attributes of its parents starting from direct parent [o].
    It checks if an overriden method (a method already defined with the same name in a parent class)
    is correctly typed: same parameters and a return type that is compatible with the overriden method.
    When there exists attributes with the same name in a parent class, we only keep the ones from the subclass.
  *)
  let rec complete (parent : identifier option) (c : clas) : clas =
    match parent with
    | None -> c
    | Some id ->
      let c' = SM.find (Location.content id) cmap in
      complete c'.extends
        {
          c with
            attributes =
            (List.filter
               (fun (name, _) ->
                 not (List.exists (fun (name', _) -> Location.content name = Location.content name') c.attributes)
               )
               c'.attributes) @ c.attributes;

            methods =
            (List.filter
               (fun (name, m) ->
                 try
                   List.find (fun (name', _) -> Location.content name = Location.content name') c.methods
                   |> test_compatible_signature (name, m);
                   false
                 with Not_found -> true
               )
               c'.methods) @ c.methods
        }
  in
  SM.map
    (fun c -> complete c.extends c)
    cmap

let typecheck_program (p : program) : TMJ.program =
  let cmap = class_map p.defs in
  let instanceof = create_instanceof cmap in
  let cenv =
    add_method cmap instanceof
    |> SM.map extract_class_type
  in
  let defs' = 
    List.map (typecheck_class cenv instanceof) p.defs
  in
  let venv = SM.singleton "this" (Typ p.name) in
  TMJ.{
    name = Location.content p.name;
    defs = defs';
    main_args = Location.content p.main_args;
    main      = fst (typecheck_instruction cenv venv S.empty instanceof p.main)
  }
  

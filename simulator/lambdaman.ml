type linstruction =
  | LLdc of int
  | LLd  of int * int
  | LAdd
  | LSub
  | LMul
  | LMiv
  | LCeq
  | LCgt
  | LCgte
  | LAtom
  | LCons
  | LCar
  | LCdr
  | LSel
  | LJoin
  | LDdf
  | LAp
  | LRtn
  | LDum
  | LStop
  | LTap
  | LTsel
  | LTrap
  | LDbug

(*
type env = {
  c: int32; (* control register *)
  s: int32; (* data stack register *)
  d: int32; (* control stack register *)
  e: int32; (* environment frame register *)

  data_stack: data_stack;
  control_stack: control_stack;
  environment_frame_chain: environment_frame_chain;
}
*)

(*
%c: control register (program counter / instruction pointer)
%s: data stack register
%d: control stack register
%e: environment frame register
*)

(*let rec eval env = function *)

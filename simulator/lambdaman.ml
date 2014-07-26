type value =
  | VInt of int32

type instruction =
  | LLdc of int32
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

type environment = {
  parent : environment option;
}

type machine = {
  mutable pc: int;
  mutable data: value Stack.t;
  mutable control: int Stack.t;
  (* mutable env: *)
}

type program = instruction array

let make_initial_machine () = {
  pc = 0;
  data = Stack.create ();
  control = Stack.create ();
}

let print_value = function
  | VInt x ->
     print_endline (Int32.to_string x)
;;

let eval machine = function
  | LLdc n ->
     Stack.push (VInt n) machine.data;
     machine.pc <- machine.pc
  | LAdd ->
     let y = Stack.pop machine.data in
     let x = Stack.pop machine.data in
     let (VInt xx) = x
     and (VInt yy) = y in
     let z = Int32.add xx yy in
     Stack.push (VInt z) machine.data;
     machine.pc <- machine.pc + 1
  | LDbug ->
     let x = Stack.pop machine.data in
     print_value x;
     machine.pc <- machine.pc + 1

(* *)
let _ =
  let machine = make_initial_machine () in
  eval machine (LLdc (Int32.of_int 1));
  eval machine (LLdc (Int32.of_int 2));
  eval machine LAdd;
  eval machine LDbug;
;;

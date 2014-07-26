type value =
  | VInt of int32

type instruction =
  | LLdc of int32
  | LLd  of int * int
  | LAdd
  | LSub
  | LMul
  | LDiv
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

let value_of_int x = VInt (Int32.of_int x)

let eval_primitive machine op =
     let y = Stack.pop machine.data in
     let x = Stack.pop machine.data in
     let (VInt xx) = x
     and (VInt yy) = y in
     let z = op xx yy in
     Stack.push (VInt z) machine.data;
     machine.pc <- machine.pc + 1
;;

let eval machine = function
  | LLdc n ->
     Stack.push (VInt n) machine.data;
     machine.pc <- machine.pc
  | LAdd ->
     eval_primitive machine Int32.add
  | LSub ->
     eval_primitive machine Int32.sub
  | LMul ->
     eval_primitive machine Int32.mul
  | LDiv ->
     eval_primitive machine Int32.div
  | LCeq ->
     eval_primitive machine (fun x y ->
       Int32.of_int (if x == y then 1 else 0)
     )
  | LCgt ->
     eval_primitive machine (fun x y ->
       Int32.of_int (if x > y then 1 else 0)
     )
  | LCgte ->
     eval_primitive machine (fun x y ->
       Int32.of_int (if x >= y then 1 else 0)
     )
  | LAtom ->
     let x = Stack.pop machine.data in
     let v = begin match x with
       | VInt _ -> VInt (Int32.of_int 1)
       | _ -> VInt (Int32.of_int 0)
     end in
     Stack.push v machine.data;
     machine.pc <- machine.pc + 1
  | LDbug ->
     let x = Stack.pop machine.data in
     print_value x;
     machine.pc <- machine.pc + 1

(* unittest *)
let _ =
  let machine = make_initial_machine () in
  eval machine (LLdc (Int32.of_int 1));
  eval machine (LLdc (Int32.of_int 2));
  eval machine LAdd;
  eval machine LDbug;
  eval machine (LLdc (Int32.of_int 3));
  eval machine LAtom;
  eval machine LDbug;
;;

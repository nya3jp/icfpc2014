open Util

exception Exception_exit

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
  | LSel  of int * int
  | LJoin
  | LLdf  of int
  | LAp   of int
  | LRtn
  | LDum  of int
  | LRap  of int
  | LTap  of int
  | LTsel of int * int
  | LTrap of int
  | LSt   of int * int
  | LDbug
  | LBrk

type value =
  | VInt     of int32
  | VCons    of value * value
  | VClosure of int * frame list

and frame = {
  mutable dummy : bool;
  mutable data : value array;
}

type address =
  | AStop
  | AJoin of int
  | ARet of int
  | AFrame of frame list

type machine = {
  mutable c: int; (* program counter *)
  mutable s: value Stack.t; (* data stack *)
  mutable d: address Stack.t; (* control stack *)
  mutable e: frame list; (* environment stack *)
}

type program = instruction array

let make_initial_machine () = {
  c = 0;
  s = Stack.create ();
  d = Stack.create ();
  e = [];
}

let check_int = function
  | VInt x -> x
  | VCons _ -> failwith "tag mismatch for int (cons came)"
  | VClosure _ -> failwith "tag mismatch for int (closure came)"

let check_cons = function
  | VInt _ -> failwith "tag mismatch for cons (int came)"
  | VCons (x, y) -> (x, y)
  | VClosure _ -> failwith "tag mismatch for cons (closure came)"

let check_closure = function
  | VInt _ -> failwith "tag mismatch for closure (int came)"
  | VCons _ -> failwith "tag mismatch for closure (cons came)"
  | VClosure (x, y) -> (x, y)

let is_closure = function
  | VClosure _ -> true
  | _ -> false

let check_tag_join = function
  | AJoin x -> x
  | _ -> failwith "tag mismatch for join"

let check_tag_ret = function
  | ARet x -> x
  | _ -> failwith "tag mismatch for ret"

let check_tag_frame = function
  | AFrame x -> x
  | _ -> failwith "tag mismatch for frame"

let is_tag_stop = function
  | AStop -> true
  | _ -> false

let is_tag_ret = function
  | ARet _ -> true
  | _ -> false

let check_not_dum e =
  if e.dummy then
    failwith "dummy"

let check_dum e =
  if not e.dummy then
    failwith "dummy"
;;

let value_of_int x = VInt (Int32.of_int x)

let alloc_frame n = {
  dummy = false;
  data = Array.make n (value_of_int 0);
}

let alloc_dummy_frame n = {
  dummy = true;
  data = Array.make n (value_of_int 0);
}

let rec string_of_value = function
  | VInt x -> Int32.to_string x
  | VCons(v1,v2) -> "(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
  | VClosure(n,_) -> "Closure{" ^ (string_of_int n) ^ "}" (* FIXME: should frame list displayed? *)
;;

let print_value v =
  print_endline (string_of_value v)

let rec get_nth_env_frame n = function
  | [] -> failwith "no environment ?"
  | e :: es ->
     if n = 0 then e
     else get_nth_env_frame (n - 1) es
;;

let set_frame_value frame i v =
  frame.data.(i) <- v

let eval_primitive machine op =
     let y = Stack.pop machine.s in
     let x = Stack.pop machine.s in
     let xx = check_int x
     and yy = check_int y in
     let z = op xx yy in
     Stack.push (VInt z) machine.s;
     machine.c <- machine.c + 1
;;

let rec eval_instruction machine = function
  | LLdc n ->
     Stack.push (VInt n) machine.s;
     machine.c <- machine.c + 1
  | LLd (n, i) ->
     let fp = get_nth_env_frame n machine.e in
     ignore(check_not_dum fp);
     let v = fp.data.(i) in
     Stack.push v machine.s;
     machine.c <- machine.c + 1
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
       Int32.of_int (if x = y then 1 else 0)
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
     let x = Stack.pop machine.s in
     let v = begin match x with
       | VInt _ -> VInt (Int32.of_int 1)
       | _ -> VInt (Int32.of_int 0)
     end in
     Stack.push v machine.s;
     machine.c <- machine.c + 1
  | LCons ->
     let y = Stack.pop machine.s in
     let x = Stack.pop machine.s in
     let z = VCons (x, y) in
     Stack.push z machine.s;
     machine.c <- machine.c + 1
  | LCar ->
     let x = Stack.pop machine.s in
     let (fst, snd) = check_cons x in
     Stack.push fst machine.s;
     machine.c <- machine.c + 1
  | LCdr ->
     let x = Stack.pop machine.s in
     let (fst, snd) = check_cons x in
     Stack.push snd machine.s;
     machine.c <- machine.c + 1
  | LSel (t, f) ->
     Stack.push (AJoin (machine.c + 1)) machine.d;
     eval_instruction machine (LTsel(t, f))
  | LJoin ->
     let x = Stack.pop machine.d in
     let xv = check_tag_join x in
     machine.c <- xv
  | LLdf f ->
     let x = VClosure (f, machine.e) in
     Stack.push x machine.s;
     machine.c <- machine.c + 1
  | LAp n ->
     Stack.push (AFrame machine.e) machine.d;
     Stack.push (ARet (machine.c + 1)) machine.d;
     eval_instruction machine (LTap(n))
  | LRtn ->
     let x = Stack.pop machine.d in
     if is_tag_stop x then
       raise Exception_exit;
     let x = check_tag_ret x in
     let y = Stack.pop machine.d in
     let yy = check_tag_frame y in
     machine.e <- yy;
     machine.c <- x;
  | LDum n ->
     let frame = alloc_dummy_frame n in
     let fp = frame :: machine.e in
     machine.e <- fp;
     machine.c <- machine.c + 1
  | LRap n ->
     Stack.push (AFrame (List.tl machine.e)) machine.d;
     Stack.push (ARet (machine.c + 1)) machine.d;
     eval_instruction machine (LTrap(n))
  | LTsel (t, f) ->
     let x = check_int (Stack.pop machine.s) in
     machine.c <- if x = Int32.zero then f else t
  | LTap n ->
     let (f, e) = check_closure(Stack.pop machine.s) in
     let fp = (alloc_frame n) :: e in
     let i = ref (n - 1) in
     while !i <> -1 do
       let y = Stack.pop machine.s in
       set_frame_value (List.hd fp) !i y;
       decr i
     done;
     machine.e <- fp;
     machine.c <- f
  | LTrap n ->
     let (f, fp) = check_closure (Stack.pop machine.s) in
     let frame = List.hd machine.e in
     ignore (check_dum frame);
     if Array.length frame.data <> n then
       failwith "frame mismatch";
     if machine.e != fp then (* physical equal *)
       failwith "frame mismatch";
     let i = ref (n - 1) in
     while !i <> -1 do
       let y = Stack.pop machine.s in
       set_frame_value (List.hd fp) !i y;
       decr i;
     done;
     let frame = List.hd fp in
     frame.dummy <- false;
     machine.e <- fp;
     machine.c <- f
  | LSt (n, i) ->
     let frame = get_nth_env_frame n machine.e in
     ignore(check_not_dum frame);
     let v = Stack.pop machine.s in
     set_frame_value frame i v;
     machine.c <- machine.c + 1
  | LDbug ->
     let x = Stack.pop machine.s in
     print_value x;
     machine.c <- machine.c + 1
  | LBrk ->
     machine.c <- machine.c + 1
;;

let eval_main program args =
  let machine = make_initial_machine () in

  (* make a frame to call main function *)
  let frame = alloc_frame (List.length args) in
  List.iteri (fun i v ->
    frame.data.(i) <- v
  ) args;
  Stack.push AStop machine.d;
  machine.e <- frame :: machine.e;
  try
    while true do
      let inst = program.(machine.c) in
      eval_instruction machine inst
    done;
    failwith "shouldn't come here"
  with
  | Exception_exit ->
     (* Returns the top value of stack. *)
     Stack.pop machine.s
;;

let eval_step program closure args =
  let (n, fp) = match closure with
    | VClosure (n, fp) -> (n, fp)
    | _ -> failwith "eval_step got non closure."
  in

  let machine = make_initial_machine () in

  let frame = alloc_frame (List.length args) in
  List.iteri (fun i v ->
    frame.data.(i) <- v
  ) args;

  machine.c <- n;
  machine.e <- frame :: fp;
  Stack.push AStop machine.d;

  try
    while true do
      print_endline (string_of_int machine.c);
      let inst = program.(machine.c) in
      eval_instruction machine inst
    done;
    failwith "shouldn't come here"
  with
  | Exception_exit ->
     (* Returns the top value of stack. *)
     Stack.pop machine.s
;;

(* ---------------------------------------------------------------------- *)

type t = {
  index: int;
  mutable x: int;
  mutable y: int;
  mutable d: direction;
  mutable vitality: int;
  mutable lives: int;
  mutable score: int;
  program: program;
}

let make index x y program = {
  index = index;
  x = x;
  y = y;
  d = Down;
  vitality = 0;
  lives = 3;
  score = 0;
  program = program;
}

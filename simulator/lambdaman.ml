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
  | LSel of int * int
  | LJoin
  | LLdf of int
  | LAp of int
  | LRtn
  | LDum
  | LStop
  | LTap
  | LTsel
  | LTrap
  | LDbug

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
  | _ -> failwith "tag mismatch for int"

let check_cons = function
  | VCons (x, y) -> (x, y)
  | _ -> failwith "tag mismatch for cons"

let check_closure v =
  match v with
  | VClosure (x, y) -> (x, y)
  | _ -> failwith "tag mismatch for closure"

let check_join = function
  | AJoin x -> x
  | _ -> failwith "tag mismatch for join"

let check_tag_ret = function
  | ARet x -> x
  | _ -> failwith "tag mismatch for ret"

let is_tag_stop = function
  | AStop -> true
  | _ -> false

let is_tag_ret = function
  | ARet _ -> true
  | _ -> false

let check_dum e =
  if e.dummy then
    failwith "dummy"
;;

let value_of_int x = VInt (Int32.of_int x)

let alloc_frame n = {
  dummy = false;
  data = Array.make n (value_of_int 0);
}

let print_value = function
  | VInt x -> print_endline (Int32.to_string x)
  | _ -> failwith "not implemented yet"
;;


let rec get_nth_env_frame n = function
  | [] -> failwith "no environment ?"
  | e :: es ->
     if n == 0 then e
     else get_nth_env_frame (n - 1) es
;;

let set_frame_value fp i v =
  match fp with
  | [] -> failwith "no frame?"
  | frame :: _ -> frame.data.(i) <- v

let eval_primitive machine op =
     let y = Stack.pop machine.s in
     let x = Stack.pop machine.s in
     let xx = check_int x
     and yy = check_int y in
     let z = op xx yy in
     Stack.push (VInt z) machine.s;
     machine.c <- machine.c + 1
;;

let eval machine = function
  | LLdc n ->
     Stack.push (VInt n) machine.s;
     machine.c <- machine.c
  | LLd (n, i) ->
     let fp = get_nth_env_frame n machine.e in
     ignore(check_dum fp);
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
     let x = Stack.pop machine.s in
     let xv = check_int x in
     Stack.push (AJoin (machine.c + 1)) machine.d;
     if xv == (Int32.of_int 1) then
       machine.c <- t
     else
       machine.c <- f
  | LJoin ->
     let x = Stack.pop machine.d in
     let xv = check_join x in
     machine.c <- xv
  | LLdf f ->
     let x = VClosure (f, machine.e) in
     Stack.push x machine.s;
     machine.c <- machine.c + 1
  | LAp n ->
     let (f, e) = check_closure(Stack.pop machine.s) in
     let fp = (alloc_frame n) :: machine.e in
     let i = ref (n - 1) in
     while !i <> -1 do
       let y = Stack.pop machine.s in
       set_frame_value fp !i y;
       decr i
     done;
     Stack.push (AFrame machine.e) machine.d;
     Stack.push (ARet (machine.c + 1)) machine.d;
     machine.e <- fp;
     machine.c <- f;
  | LRtn ->
     let x = Stack.pop machine.d in
     if is_tag_stop x then
       raise Exit;
     let x = check_tag_ret x in
     let y = Stack.pop machine.d in
     let (AFrame yy) = y in
     machine.e <- yy;
     machine.c <- x;
  | LDbug ->
     let x = Stack.pop machine.s in
     print_value x;
     machine.c <- machine.c + 1

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

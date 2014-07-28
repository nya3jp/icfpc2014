open Util

exception Exception_exit
exception Exception_cycleover
exception Exception_eval_error of string

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

let string_of_inst = function
  | LLdc(i) -> "Ldc " ^ Int32.to_string i
  | LLd(a,b) -> "Ld " ^ string_of_int a ^ " " ^ string_of_int b
  | LAdd -> "Add"
  | LSub -> "Sub"
  | LMul -> "Mul"
  | LDiv -> "Div"
  | LCeq -> "Ceq"
  | LCgt -> "Cgt"
  | LCgte -> "Cgte"
  | LAtom -> "Atom"
  | LCons -> "Cons"
  | LCar -> "Car"
  | LCdr -> "Cdr"
  | LSel(a,b) -> "Sel " ^ string_of_int a ^ " " ^ string_of_int b
  | LJoin -> "Join"
  | LLdf(a) -> "Ldf " ^ string_of_int a
  | LAp(a) -> "Ap " ^ string_of_int a
  | LRtn  -> "Rtn"
  | LDum(a)  -> "Dum " ^ string_of_int a
  | LRap(a)  -> "Rap " ^ string_of_int a
  | LTap(a)  -> "Tap " ^ string_of_int a
  | LTsel(a,b) -> "Tsel " ^ string_of_int a ^ " " ^ string_of_int b
  | LTrap(a) -> "Trap " ^ string_of_int a
  | LSt(a,b)   -> "St " ^ string_of_int a ^ " " ^ string_of_int b
  | LDbug -> "Dbug"
  | LBrk  -> "Brk"


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
  debug_callback: unit -> unit (* will be called when debug *)
}

type program = instruction array

let make_initial_machine debug_callback = {
  c = 0;
  s = Stack.create ();
  d = Stack.create ();
  e = [];
  debug_callback = debug_callback;
}

let eval_error string =
  raise (Exception_eval_error string)

let check_int = function
  | VInt x -> x
  | VCons _ -> eval_error "tag mismatch for int (cons came)"
  | VClosure _ -> eval_error "tag mismatch for int (closure came)"

let check_cons = function
  | VInt _ -> eval_error "tag mismatch for cons (int came)"
  | VCons (x, y) -> (x, y)
  | VClosure _ -> eval_error "tag mismatch for cons (closure came)"

let check_closure = function
  | VInt _ -> eval_error "tag mismatch for closure (int came)"
  | VCons _ -> eval_error "tag mismatch for closure (cons came)"
  | VClosure (x, y) -> (x, y)

let is_closure = function
  | VClosure _ -> true
  | _ -> false

let check_tag_join = function
  | AJoin x -> x
  | _ -> eval_error "tag mismatch for join"

let check_tag_ret = function
  | ARet x -> x
  | _ -> eval_error "tag mismatch for ret"

let check_tag_frame = function
  | AFrame x -> x
  | _ -> eval_error "tag mismatch for frame"

let is_tag_stop = function
  | AStop -> true
  | _ -> false

let is_tag_ret = function
  | ARet _ -> true
  | _ -> false

let check_not_dum e =
  if e.dummy then
    eval_error "dummy"

let check_dum e =
  if not e.dummy then
    eval_error "dummy"
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

let string_of_address = function
  | AStop -> "stop"
  | AJoin x -> ("join:" ^ (string_of_int x))
  | ARet x -> ("ret:" ^ (string_of_int x))
  | AFrame fp -> ("frame")
;;

let string_of_frame frame =
  let buf = Buffer.create 10 in
  Buffer.add_string buf "dummy=";
  Buffer.add_string buf (if frame.dummy then "t" else "f");
  Buffer.add_string buf " / ";
  Buffer.add_string buf (String.concat " " (Array.to_list (Array.map (fun v -> string_of_value v) frame.data)));
  Buffer.contents buf
;;

let print_value v =
  print_endline (string_of_value v)

let print_machine machine =
  Printf.printf "c = %d\n" machine.c;
  Printf.printf "s = %s\n" (
    let buf = Buffer.create 10 in
    Stack.iter (fun v ->
      Buffer.add_string buf (string_of_value v);
      Buffer.add_string buf " ";
    ) machine.s;
    Buffer.contents buf
  );
  Printf.printf "d = %s\n" (
    let buf = Buffer.create 10 in
    Stack.iter (fun a ->
      Buffer.add_string buf (string_of_address a);
      Buffer.add_string buf " ";
    ) machine.d;
    Buffer.contents buf
  );
  Printf.printf "e = %s\n" (
    String.concat " / " (List.map (fun frame ->
      (string_of_frame frame);
    ) machine.e)
  )
;;

let rec get_nth_env_frame n = function
  | [] -> eval_error "no environment ?"
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
     eval_primitive machine div_int32_toward_negative
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
       | VInt _ -> value_of_int 1
       | _ -> value_of_int 0
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
       eval_error "frame mismatch";
     if machine.e != fp then (* physical equal *)
       eval_error "frame mismatch";
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
     print_string "; trace lambdaman: ";
     print_value x;
     machine.debug_callback ();
     machine.c <- machine.c + 1
  | LBrk ->
     print_endline "DEBUG BREAK";
     print_machine machine;
     print_endline "DEBUG BREAK DONE";
     machine.c <- machine.c + 1
;;

let eval_program program =
  let machine = make_initial_machine (fun () -> ()) in
  Stack.push AStop machine.d;
  try
    for i = 0 to 3072000 - 1 do
      let inst = program.(machine.c) in
      eval_instruction machine inst
    done;
  with
  | Exception_exit ->
     print_machine machine
;;

let show_profile program profile =
  let total = Array.fold_left (+) 0 profile in
  let a2 = (Array.mapi (fun i count -> (-count,i)) profile) in
  Array.sort Pervasives.compare a2;
  ignore (Array.fold_left
    (fun (nth,cum_count) (count,addr) ->
      let count = - count in
      if nth < 1000 && count > 0 && cum_count < total * 99 / 100 then
        Printf.printf "Addr %8d: count=%8d (%5.1f%% cum=%5.1f%%) %s\n"
          addr
          count
          (float_of_int count *. 100.0 /. float_of_int total)
          (float_of_int (cum_count+count) *. 100.0 /. float_of_int total)
          (string_of_inst program.(addr))
      ;
      (nth+1,cum_count+count)
    )
    (0,0)
    a2
  );
  Printf.printf "Total: %d inst\n" total

type t = {
  index: int;
  mutable x: int;
  mutable y: int;
  initialX: int;
  initialY: int;
  mutable d: direction;
  mutable vitality_absolute: int;
  mutable eat_count: int;
  mutable lives: int;
  mutable score: int;
  program: program;
  mutable state: value;
  mutable stepFun: value;
  mutable profile_total: int array;
}


let eval_main man show_useful_info use_eternal program args =
  let i = ref 0 in
  let debug_callback =
    let last = ref 0 in
    function () ->
      if show_useful_info then begin
        print_endline ("Current  Cycle: " ^ (string_of_int !i));
        print_endline ("Previous Cycle: " ^ (string_of_int !last));
        print_endline ("Diff          ; " ^ (string_of_int (!last - !i)));
      end;
      last := !i
  in

  let machine = make_initial_machine debug_callback in

  (* make a frame to call main function *)
  let frame = alloc_frame (List.length args) in
  List.iteri (fun i v ->
    frame.data.(i) <- v
  ) args;
  Stack.push AStop machine.d;
  machine.e <- frame :: machine.e;
  let profile_total = Array.make (Array.length program) 0 in
  man.profile_total <- profile_total;
  let profile = Array.make (Array.length program) 0 in
  try
    let maxSteps = 3072000 * 60 in
    while !i < maxSteps || use_eternal do
      profile.(machine.c) <- profile.(machine.c) + 1;
      profile_total.(machine.c) <- profile_total.(machine.c) + 1;
      let inst = program.(machine.c) in
      eval_instruction machine inst;
      incr i
    done;

    failwith (Printf.sprintf "Over cycle limit! main function didn't end after %d cycles\n" maxSteps)
  with
  | Exception_exit ->
     if show_useful_info then begin
       Printf.printf "Used %d cycles.\n" !i;
       (* show_profile program profile; *)
     end;
     (* Returns the top value of stack. *)
     Stack.pop machine.s
  | Exception_eval_error reason ->
     Printf.printf "Evaluation Error: %s\n" reason;
     Printf.printf "Current pc = %d\n" machine.c;
     failwith "Eval error in main function"
;;

let eval_step man show_useful_info use_eternal program closure args =
  let (n, fp) = match closure with
    | VClosure (n, fp) -> (n, fp)
    | _ -> failwith "eval_step got non closure."
  in

  let i = ref 0 in
  let debug_callback =
    let last = ref 0 in
    function () ->
      if show_useful_info then begin
        print_endline ("Current  Cycle: " ^ (string_of_int !i));
        print_endline ("Previous Cycle: " ^ (string_of_int !last));
        print_endline ("Diff          ; " ^ (string_of_int (!last - !i)));
      end;
      last := !i
  in

  let machine = make_initial_machine debug_callback in

  let frame = alloc_frame (List.length args) in
  List.iteri (fun i v ->
    frame.data.(i) <- v
  ) args;

  machine.c <- n;
  machine.e <- frame :: fp;
  Stack.push AStop machine.d;
  let profile = Array.make (Array.length program) 0 in
  let profile_total = man.profile_total in
  try
    let maxSteps = 3072000 in
    while !i < maxSteps || use_eternal do
      profile.(machine.c) <- profile.(machine.c) + 1;
      profile_total.(machine.c) <- profile_total.(machine.c) + 1;
      let inst = program.(machine.c) in
      eval_instruction machine inst;
      incr i
    done;

    if show_useful_info then begin
      Printf.printf "Over cycle limit! step function didn't end after %d cycles\n" maxSteps;
    end;
    raise Exception_cycleover
  with
  | Exception_exit ->
     if show_useful_info then begin
       Printf.printf "Used %d cycles.\n" !i;
       (* show_profile program profile; *)
     end;
     (* Returns the top value of stack. *)
     Stack.pop machine.s
  | Exception_eval_error reason ->
     Printf.printf "Evaluation Error: %s\n" reason;
     Printf.printf "Current pc = %d\n" machine.c;
     raise (Exception_eval_error reason)
;;

(* ---------------------------------------------------------------------- *)

let make index x y program = {
  index = index;
  x = x;
  y = y;
  initialX = x;
  initialY = y;
  d = Down;
  vitality_absolute = -1;
  eat_count = 0;
  lives = 3;
  score = 0;
  program = program;
  state = value_of_int 12345; (* dummy *)
  stepFun = value_of_int 23456; (* dummy *)
  profile_total = Array.make 0 0; (* dummy *)
}

let eaten lambdaman =
  lambdaman.lives <- lambdaman.lives - 1;
  if lambdaman.lives > 0 then begin
    lambdaman.x <- lambdaman.initialX;
    lambdaman.y <- lambdaman.initialY;
    lambdaman.d <- Down
  end

let get_vitality lambdaman tick = max 0 (lambdaman.vitality_absolute - tick)
let get_vitality_raw lambdaman tick = lambdaman.vitality_absolute - tick

let move lambdaman d =
  let revert = (lambdaman.d, lambdaman.x, lambdaman.y) in
  lambdaman.d <- d;
  begin match d with
  | Up -> lambdaman.y <- lambdaman.y - 1
  | Down -> lambdaman.y <- lambdaman.y + 1
  | Left -> lambdaman.x <- lambdaman.x - 1
  | Right -> lambdaman.x <- lambdaman.x + 1
  end;
  revert

let revert_move conf_lambdaman_invalid_move_mode lambdaman (d,x,y) =
  if conf_lambdaman_invalid_move_mode then
    lambdaman.d <- Up
  else
    lambdaman.d <- d
  ;
  lambdaman.x <- x;
  lambdaman.y <- y

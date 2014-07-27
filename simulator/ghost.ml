open Util

exception Halt_exception

type gvalue =
  | GVConst    of int
  | GVReg      of int
  | GVPCReg
  | GVIndConst of int
  | GVIndReg   of int

type ginstruction =
  | GMov of gvalue * gvalue
  | GInc of gvalue
  | GDec of gvalue
  | GAdd of gvalue * gvalue
  | GSub of gvalue * gvalue
  | GMul of gvalue * gvalue
  | GDiv of gvalue * gvalue
  | GAnd of gvalue * gvalue
  | GOr  of gvalue * gvalue
  | GXor of gvalue * gvalue
  | GJlt of int * gvalue * gvalue
  | GJeq of int * gvalue * gvalue
  | GJgt of int * gvalue * gvalue
  | GInt of int
  | GHlt

type genv = {
  mutable reg : int array;
  mutable pc  : int;
  mutable data: int array;
  mutable newDir: int;
}

type gprogram = ginstruction array

(* ---------------------------------------------------------------------- *)

let make_initial_genv () =
  {
    reg  = Array.make 8 0;
    pc   = 0;
    data = Array.make 256 0;
    newDir  = 0;
  }

let eval_gvalue env = function
  | GVConst    x -> x
  | GVReg      r -> env.reg.(r)
  | GVPCReg      -> env.pc
  | GVIndConst x -> env.data.(x)
  | GVIndReg   r -> env.data.(r)

let rec eval_ginstruction env syscallback = function
  | GMov (x1, x2) ->
     let v2 = eval_gvalue env x2 in
     set_gvalue env x1 v2
  | GInc x ->
     let vx = eval_gvalue env x in
     set_gvalue env x ((vx + 1) land 0xFF)
  | GDec x ->
     let vx = eval_gvalue env x in
     set_gvalue env x ((vx - 1) land 0xFF)
  | GAdd (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x ((vx + vy) land 0xFF);
     env.pc <- env.pc + 1;
  | GSub (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x ((vx - vy) land 0xFF);
     env.pc <- env.pc + 1;
  | GMul (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x ((vx * vy) land 0xFF);
     env.pc <- env.pc + 1;
  | GDiv (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x ((vx / vy) land 0xFF);
     env.pc <- env.pc + 1;
  | GAnd (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x ((vx land vy) land 0xFF);
     env.pc <- env.pc + 1;
  | GOr (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x (vx lor vy);
     env.pc <- env.pc + 1
  | GXor (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x ((vx lxor vy) land 0xFF);
     env.pc <- env.pc + 1;
  | GJlt (t, x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     if vx < vy then
       env.pc <- t
     else
       env.pc <- env.pc + 1
  | GJeq (t, x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     if vx = vy then
       env.pc <- t
     else
       env.pc <- env.pc + 1
  | GJgt (t, x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     if vx > vy then
       env.pc <- t
     else
       env.pc <- env.pc + 1
  | GInt n when 0 <= n && n <= 8 ->
     syscallback n env;
  | GInt _ ->
     failwith "Unknown syscall"
  | GHlt ->
     raise Halt_exception
and set_gvalue env dst v =
  match dst with
  | GVConst _ ->
     failwith "set_gvalue: cannot set to constant"
  | GVReg r ->
     env.reg.(r) <- v
  | GVPCReg ->
     env.pc <- v
  | GVIndConst x ->
     env.data.(x) <- v
  | GVIndReg r ->
     env.data.(env.reg.(r)) <- v
;;

(* ---------------------------------------------------------------------- *)

type vitality =
  | Standard
  | FrightMode (* TODO: should be here? *)
  | Invisible

let int_of_vitality = function
  | Standard -> 0
  | FrightMode -> 1
  | Invisible -> 2
;;

type t = {
  index: int;
  mutable x: int;
  mutable y: int;
  initialX: int;
  initialY: int;
  mutable d: direction; (* direction *)
  mutable vitality: vitality;
  program: gprogram;
  env: genv;
}

let reset ghost =
  ghost.x <- ghost.initialX;
  ghost.y <- ghost.initialY;
  ghost.d <- Down

let eaten ghost =
  reset ghost;
  ghost.vitality <- Invisible

let make index x y program = {
  index = index;
  x = x;
  y = y;
  initialX = x;
  initialY = y;
  d = Down;
  vitality = Standard;
  program = program;
  env = make_initial_genv ()
}

(* eval and return the new direction. *)
let eval t syscallback =
  (* ghost only consumes 1024 instructions *)
  t.env.pc <- 0;
  begin
    try
      for i = 0 to 1023 do
        eval_ginstruction t.env syscallback t.program.(t.env.pc)
      done;
    with
    | Halt_exception -> ()
  end;
  t.env.newDir
;;

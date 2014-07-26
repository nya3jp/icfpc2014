(*
  ghost is a
*)

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
  | GLlt of int * gvalue * gvalue
  | GJeq of int * gvalue * gvalue
  | GJgt of int * gvalue * gvalue
  | GInt of int
  | GHlt

type genv = {
  mutable reg : int array;
  mutable pc  : int;
  mutable data: int array;
}

type gprogram = ginstruction array

let make_initial_genv () =
  {
    reg  = Array.make 8 0;
    pc   = 0;
    data = Array.make 256 0;
  }

let eval_gvalue env = function
  | GVConst    x -> x
  | GVReg      r -> env.reg.(r)
  | GVPCReg      -> env.pc
  | GVIndConst x -> env.data.(x)
  | GVIndReg   r -> env.data.(r)
;;

let rec geval_instruction env = function
  | GMov (x1, x2) ->
     failwith "not implemented"
  | GInc x ->
     failwith "not implemented"
  | GDec x ->
     failwith "not implemented"
  | GAdd (x, y) ->
     failwith "not implemented"
  | GSub (x, y) ->
     failwith "not implemented"
  | GMul (x, y) ->
     failwith "not implemented"
  | GDiv (x, y) ->
     failwith "not implemented"
  | GAnd (x, y) ->
     failwith "not implemented"
  | GOr (x, y) ->
     let vx = eval_gvalue env x
     and vy = eval_gvalue env y in
     set_gvalue env x (vx lor vy);
     env.pc <- env.pc + 1
  | GXor (x, y) ->
     failwith "not implemented"
  | GLlt (t, x, y) ->
     failwith "not implemented"
  | GJeq (t, x, y) ->
     failwith "not implemented"
  | GJgt (t, x, y) ->
     failwith "not implemented"
  | GInt x ->
     failwith "not implemented"
  | GHlt ->
     ()
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


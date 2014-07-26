open Util

(* simulator state *)
type t = {
  lambdamans: Lambdaman.t array;
  ghosts:     Ghost.t array;
  field:      Field.t;
  mutable fruit_exists: bool;
  mutable pill_count: int;
  mutable powerpill_count: int;
}

let make field lambdaman_programs ghost_programs =
  if Array.length lambdaman_programs <= 0 || 2 < Array.length lambdaman_programs then
    failwith "# of lambdaman program is wrong";

  if Array.length ghost_programs <= 0 || 4 < Array.length ghost_programs then
    failwith "# of ghost program is wrong";

  let lambdaman_index = ref 0
  and ghost_index = ref 0 in

  let lambdamans = ref []
  and ghosts = ref [] in

  let pill_cnt = ref 0
  and powerpill_cnt = ref 0 in

  Array.iteri ( fun y line ->
    Array.iteri (fun x cell ->
      match cell with
      | Field.CPill ->
         incr pill_cnt
      | Field.CPowerPill ->
         incr powerpill_cnt
      | Field.CLambdaManStart ->
         let t = Lambdaman.make !lambdaman_index x y lambdaman_programs.(!lambdaman_index) in
         lambdamans := t :: !lambdamans;
         incr lambdaman_index
      | Field.CGhostStart ->
         let t = Ghost.make !ghost_index x y ghost_programs.(!ghost_index mod (Array.length ghost_programs)) in
         ghosts := t :: !ghosts;
         incr ghost_index
      | _ ->
         (* Currently we don't do anything, Is there anything to do here? *)
         ()
    ) line
  ) field;

  {
    lambdamans = Array.of_list (List.rev !lambdamans);
    ghosts = Array.of_list (List.rev !ghosts);
    field = field;
    fruit_exists = false;
    pill_count = !pill_cnt;
    powerpill_count = !powerpill_cnt
  }
;;

(* This is a callback when INT is called from ghost. *)
let make_syscallback_for_ghost (t : t) (ghost : Ghost.t) =
  let syscallback_for_ghost n env =
    match n with
    | 0 ->
       env.Ghost.newDir <- env.Ghost.reg.(0)
    | 1 ->
       env.Ghost.reg.(0) <- t.lambdamans.(0).x;
       env.Ghost.reg.(1) <- t.lambdamans.(0).y
    | 2 ->
       if Array.length t.lambdamans < 2 then begin
         env.Ghost.reg.(0) <- t.lambdamans.(1).Lambdaman.x;
         env.Ghost.reg.(1) <- t.lambdamans.(1).Lambdaman.y
       end
    | 3 ->
       env.Ghost.reg.(0) <- ghost.index
    | 4 ->
       let idx = env.reg.(0) in
       env.Ghost.reg.(0) <- t.ghosts.(idx).Ghost.initialX;
       env.Ghost.reg.(1) <- t.ghosts.(idx).Ghost.initialY;
    | 5 ->
       let idx = env.reg.(0) in
       env.Ghost.reg.(0) <- t.ghosts.(idx).Ghost.x;
       env.Ghost.reg.(1) <- t.ghosts.(idx).Ghost.y;
    | 6 ->
       let idx = env.reg.(0) in
       env.Ghost.reg.(0) <- Ghost.int_of_vitality t.ghosts.(idx).vitality;
       env.Ghost.reg.(1) <- int_of_direction (t.ghosts.(idx).d);
    | 7 ->
       let x = env.reg.(0)
       and y = env.reg.(1) in
       env.Ghost.reg.(0) <- Field.int_of_cell (Field.get t.field ~y ~x)
    | 8 ->
       Printf.printf "%d %d %d %d %d %d %d %d %d\n"
         env.Ghost.pc
         env.Ghost.reg.(0) env.Ghost.reg.(1) env.Ghost.reg.(2) env.Ghost.reg.(3)
         env.Ghost.reg.(4) env.Ghost.reg.(5) env.Ghost.reg.(6) env.Ghost.reg.(7)
    | n ->
       failwith ("Unknown INT number" ^ (string_of_int n))
  in
  syscallback_for_ghost
;;

(*---- SCORE TABLE ----*)

let score_pill = 10
let score_power_pill = 50
let score_fruit field =
  let scores = [| 0; 100; 300; 500; 500; 700; 700; 1000; 1000; 2000; 2000; 3000; 3000; |] in
  let level = Field.level_of_field field in
  if level < 0 then
    failwith ("Unexpected field level: " ^ (string_of_int level));
  if level < 12 then
    scores.(level)
  else
    5000
;;

(*---- TICK TABLE ----*)

type event_type =
  | FruitAppear
  | FruitDisappear
  | LambdaManMove of int
  | GhostMove of int

(*
let tick tick_id t =
  move_lambdamans_if_necessary tick_id t;
  move_ghosts_if_necessary tick_id t;
  check_fright_mode_deactivating tick_id t;
  check_fruit_appearing tick_id t;
*)

(* ---------------------------------------------------------------------- *)

let encode_field t = failwith "not implemented yet"
let encode_status t = failwith "not implemented yet"
let encode_ghost t = failwith "not implemented yet"
let encode_fruit t = failwith "not implemented yet"

let encode_current_world t =
  let field_encoded = encode_field t in
  let status_encoded = encode_status t in
  let status_ghost = encode_ghost t in
  let status_fruit = encode_fruit t in
  let zero = Lambdaman.value_of_int 0 in
  List.fold_right (fun x y -> Lambdaman.VCons (x, y)) [field_encoded; status_encoded; status_ghost; status_fruit] zero
;;

(* TODO: implement this. Encode HLT now. *)
let encode_ghost_program program =
  Lambdaman.VCons (Lambdaman.value_of_int 14, Lambdaman.value_of_int 0)
;;

let encode_ghost_programs t =
  let encoded = Array.map (fun ghost -> encode_ghost_program ghost.Ghost.program) t.ghosts in
  Array.fold_right (fun x y -> Lambdaman.VCons (x, y)) encoded (Lambdaman.VInt (Int32.of_int 0))
;;

let run t =
  (* First, run lambdaman and get *)
  failwith "not implemented yet"
;;

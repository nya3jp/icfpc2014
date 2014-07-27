open Util

module OrderedEventType = struct
  type t = int * int * int
  let compare : t -> t -> int = Pervasives.compare
end;;
module TQ = Set.Make(OrderedEventType);; (* tick, eventID, eventArg; in acsending order *)

(* simulator state *)
type t = {
  lambdamans: Lambdaman.t array;
  ghosts:     Ghost.t array;
  field:      Field.t;
  mutable fruit_exists: bool;
  mutable pill_count: int;
  mutable powerpill_count: int;
  mutable wl: TQ.t; (* priority queue of tick events *)
}

(*---- SCORE TABLE ----*)
let score_pill = 10
let score_power_pill = 50
let score_fruit field = match Field.level_of_field field with
  |  1 ->  100
  |  2 ->  300
  |  3 ->  500
  |  4 ->  500
  |  5 ->  700
  |  6 ->  700
  |  7 -> 1000
  |  8 -> 1000
  |  9 -> 2000
  | 10 -> 2000
  | 11 -> 3000
  | 12 -> 3000
  | n when n > 12 -> 5000
  | _ -> failwith "illegal level"

let score_eat = function
  | 1 ->  200
  | 2 ->  400
  | 3 ->  800
  | n when n >= 3 -> 1600
  | _ -> failwith "illegal nth"

(*---- TICK TABLE ----*)
let tick_EOL field = 127 * Field.width_of_field field * Field.height_of_field field * 16
let tick_fruit_appear id (* 0 or 1 *) = 127 * 200 * (id+1)
let tick_fruit_disappear id (* 0 or 1 *) = 127 * 200 * (id+1) + 80
let tick_dur_fright = 127 * 20
let tick_move_lambdaman is_eating = if is_eating then 137 else 127
let tick_move_ghost ghost =
  if ghost.Ghost.vitality = Ghost.FrightMode then
    [|195; 198; 201; 204|].(ghost.Ghost.index mod 4)
  else
    [|130; 132; 134; 136|].(ghost.Ghost.index mod 4)

let eLambdamanMove     = 100 (* lambdaman id *)
let eGhostMove         = 101 (* ghost *)
let eFruitAppear       = 200
let eFruitDisappear    = 201
let eFrightDeactivate = 202
let eLambdamanPostprocess  = 300
let eEOL = 999999
;;

let schedule_tick world v = world.wl <- TQ.add v world.wl

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

  let wl = ref TQ.empty in

  wl := TQ.add (tick_EOL field, eEOL, 0) !wl;
  List.iter (fun lambdaman -> wl := TQ.add (tick_move_lambdaman false, eLambdamanMove, lambdaman.Lambdaman.index) !wl) !lambdamans;
  List.iter (fun ghost -> wl := TQ.add (tick_move_ghost ghost, eGhostMove, ghost.Ghost.index) !wl) !ghosts;
  List.iter (fun i -> wl := TQ.add (tick_fruit_appear i, eFruitAppear, 0) !wl) [0; 1];
  List.iter (fun i -> wl := TQ.add (tick_fruit_disappear i, eFruitDisappear, 0) !wl) [0; 1];

  {
    lambdamans = Array.of_list (List.rev !lambdamans);
    ghosts = Array.of_list (List.rev !ghosts);
    field = field;
    fruit_exists = false;
    pill_count = !pill_cnt;
    powerpill_count = !powerpill_cnt;
    wl = !wl;
  }
;;

let next_tick world =
  let (tick, event_id, event_arg) = TQ.min_elt world.wl in
  begin match event_id with
  | x when x == eFruitAppear ->
      world.fruit_exists <- true;
  | x when x == eFruitDisappear ->
      world.fruit_exists <- false;
  | x when x == eEOL ->
      raise Exit (* FIXME *)
  | x when x == eLambdamanMove ->
      let lambdaman = world.lambdamans.(event_arg) in
      (* FIXME: run program *)
      (* FIXME: move lambdaman *)
      let is_eating = begin match world.field.(lambdaman.Lambdaman.y).(lambdaman.Lambdaman.x) with
      | Field.CPill | Field.CPowerPill -> true
      | Field.CFruitLocation -> world.fruit_exists
      | _ -> false
      end in
      schedule_tick world (tick, eLambdamanPostprocess, event_arg);
      schedule_tick world ((tick + tick_move_lambdaman is_eating), eLambdamanMove, event_arg)
  | x when x == eGhostMove ->
      let ghost = world.ghosts.(event_arg) in
      (* FIXME: run program *)
      (* FIXME: move lambdaman *)
      schedule_tick world (tick, eLambdamanPostprocess, 0); (* FIXME *)
      schedule_tick world ((tick + tick_move_ghost ghost), eGhostMove, event_arg)
  | x when x == eFrightDeactivate ->
      let lambdaman = world.lambdamans.(event_arg) in
      if tick == lambdaman.Lambdaman.vitality_absolute then begin
        Array.iter
          (fun g -> g.Ghost.vitality <- Ghost.Standard)
          world.ghosts
      end
  | x when x == eLambdamanPostprocess ->
      let lambdaman = world.lambdamans.(event_arg) in
      (* Step 3 *)
      begin match world.field.(lambdaman.Lambdaman.y).(lambdaman.Lambdaman.x) with
      | Field.CPill ->
          lambdaman.Lambdaman.score <- lambdaman.Lambdaman.score + score_pill;
          world.pill_count <- world.pill_count - 1;
          world.field.(lambdaman.Lambdaman.y).(lambdaman.Lambdaman.x) <- Field.CEmpty
      | Field.CPowerPill ->
          lambdaman.Lambdaman.score <- lambdaman.Lambdaman.score + score_power_pill;
          world.powerpill_count <- world.powerpill_count - 1;
          (* FIXME: activate fright mode *)
          lambdaman.Lambdaman.vitality_absolute <- tick + tick_dur_fright;
          lambdaman.Lambdaman.eat_count <- 0;
          Array.iter
            (fun g ->
              if g.Ghost.vitality == Ghost.Standard then g.Ghost.vitality <- Ghost.FrightMode;
              g.Ghost.d <- reverse_direction g.Ghost.d (* FIXME: correct? *)
            )
            world.ghosts;
          schedule_tick world (tick + tick_dur_fright, eFrightDeactivate, event_arg);
          world.field.(lambdaman.Lambdaman.y).(lambdaman.Lambdaman.x) <- Field.CEmpty
      | Field.CFruitLocation ->
          if world.fruit_exists then begin
            lambdaman.Lambdaman.score <- lambdaman.Lambdaman.score + score_fruit world.field;
            world.fruit_exists <- false
          end
      | _ -> ()
      end;
      (* Step 4 *)
      Array.iter
        (fun g ->
          if g.Ghost.x == lambdaman.Lambdaman.x && g.Ghost.y == lambdaman.Lambdaman.y then match g.Ghost.vitality with
          | Ghost.Standard -> (* Eaten... *)
              Lambdaman.eaten lambdaman;
              Array.iter Ghost.reset world.ghosts
          | Ghost.FrightMode -> (* Eat ghost! *)
              Ghost.eaten g;
              lambdaman.Lambdaman.eat_count <- lambdaman.Lambdaman.eat_count + 1;
              lambdaman.Lambdaman.score <- lambdaman.Lambdaman.score + score_eat lambdaman.Lambdaman.eat_count
          | Ghost.Invisible -> () (* do nothing *)
        )
        world.ghosts;
      (* Step 5 *)
      if world.pill_count == 0 then
        failwith "Win" (* FIXME *)
      ;
      (* Step 6 *)
      if lambdaman.Lambdaman.lives <= 0 then
        failwith "Lose" (* FIXME *)
      ;
  | _ -> failwith "invalid event_id"
  end


(* This is a callback when INT is called from ghost. *)
let make_syscallback_for_ghost (t : t) (ghost : Ghost.t) =
  let syscallback_for_ghost n env =
    match n with
    | 0 ->
       env.Ghost.newDir <- env.Ghost.reg.(0)
    | 1 ->
       env.Ghost.reg.(0) <- t.lambdamans.(0).Lambdaman.x;
       env.Ghost.reg.(1) <- t.lambdamans.(0).Lambdaman.y
    | 2 ->
       if Array.length t.lambdamans < 2 then begin
         env.Ghost.reg.(0) <- t.lambdamans.(1).Lambdaman.x;
         env.Ghost.reg.(1) <- t.lambdamans.(1).Lambdaman.y
       end
    | 3 ->
       env.Ghost.reg.(0) <- ghost.Ghost.index
    | 4 ->
       let idx = env.Ghost.reg.(0) in
       env.Ghost.reg.(0) <- t.ghosts.(idx).Ghost.initialX;
       env.Ghost.reg.(1) <- t.ghosts.(idx).Ghost.initialY;
    | 5 ->
       let idx = env.Ghost.reg.(0) in
       env.Ghost.reg.(0) <- t.ghosts.(idx).Ghost.x;
       env.Ghost.reg.(1) <- t.ghosts.(idx).Ghost.y;
    | 6 ->
       let idx = env.Ghost.reg.(0) in
       env.Ghost.reg.(0) <- Ghost.int_of_vitality t.ghosts.(idx).Ghost.vitality;
       env.Ghost.reg.(1) <- int_of_direction (t.ghosts.(idx).Ghost.d);
    | 7 ->
       let x = env.Ghost.reg.(0)
       and y = env.Ghost.reg.(1) in
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

(* ---------------------------------------------------------------------- *)

open Lambdaman

let rec encode_as_tuple = function
  | [] -> failwith ""
  | x :: [] -> x
  | x :: xs -> VCons (x, encode_as_tuple xs)
;;

let rec encode_as_list = function
  | [] -> value_of_int 0
  | x :: xs -> VCons (x, encode_as_list xs)
;;

let encode_as_list list =
  encode_as_tuple list
;;

let encode_field field =
  let zero = value_of_int 0 in
  Array.fold_right (fun x y ->
    let x' = Array.fold_right (fun cell t ->
      let v = value_of_int (Field.int_of_cell cell) in
      VCons (v, t)
    ) x zero in
    VCons (x', y)
  ) field zero
;;

(* Consider only single lambdaman? *)
let encode_status t tick =
  let man = t.lambdamans.(0) in
  let vitality = Lambdaman.value_of_int (Lambdaman.get_vitality man tick) in
  let position = VCons (value_of_int man.x, value_of_int man.y) in
  let dirrection = value_of_int (int_of_direction man.d) in
  let lives = value_of_int man.lives in
  let score = value_of_int man.score in
  encode_as_tuple [vitality; position; dirrection; lives; score]
;;

let encode_ghost t =
  let m = Array.map (fun ghost ->
    let vitality = value_of_int (Ghost.int_of_vitality ghost.Ghost.vitality) in
    let location = VCons (value_of_int ghost.Ghost.x, value_of_int ghost.Ghost.y) in
    let direction = value_of_int (int_of_direction ghost.Ghost.d) in
    encode_as_tuple [vitality; location; direction]
  ) t.ghosts in
  encode_as_list (Array.to_list m)
;;

(* fruit might exist in [127 * 200, 127 * 280], [127 * 400, 127 * 480] *)
let encode_fruit t tick =
  let v =
    if 127 * 200 <= tick && tick <= 127 * 280 && t.fruit_exists then
      127 * 280 - tick
    else if 127 * 400 <= tick && tick <= 127 * 480 && t.fruit_exists then
      127 * 480 - tick
    else
      0
  in
  value_of_int v
;;

let encode_current_world t tick =
  let field_encoded = encode_field t.field in
  let status_encoded = encode_status t tick in
  let status_ghost = encode_ghost t in
  let status_fruit = encode_fruit t tick in
  encode_as_tuple [field_encoded; status_encoded; status_ghost; status_fruit]
;;

(* TODO: implement this. Encode HLT now. *)
let encode_ghost_program program =
  VCons (value_of_int 14, value_of_int 0)
;;

let encode_ghost_programs t =
  let encoded = Array.map (fun ghost -> encode_ghost_program ghost.Ghost.program) t.ghosts in
  Array.fold_right (fun x y -> VCons (x, y)) encoded (VInt (Int32.of_int 0))
;;

let run t =
  (* First, call lambdaman main. *)
  let state = encode_current_world t 0
  and ghosts = encode_ghost_programs t in
  let v = Array.map (fun man ->
    eval_main man.program [state; ghosts]
  ) t.lambdamans in

  print_value v.(0);

  (* checking step func is callable. *)
  let (VCons (state, stepFun)) = v.(0) in
  print_value stepFun;
  let _ =
    let VClosure (n, fp) = stepFun in
    print_endline ("CLOSURE: " ^ (string_of_int (List.length fp)));
  in
  let v = eval_step t.lambdamans.(0).program stepFun [state; encode_current_world t 1] in
  print_value v;

  (* Then, each next tick, call step and state. *)
  failwith "not implemented yet"
;;

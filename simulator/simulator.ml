(* simulator code will be here. *)

module OrderedEventType = struct
  type t = int * int * int
  let compare = Pervasives.compare
end;;
module TQ = Set.Make(OrderedEventType);; (* tick, eventID, eventArg; in acsending order *)

(* simulator state *)
type t = {
  lambdamans: Lambdaman.t array;
  ghosts:     Ghost.t array;
  field:      Field.t;
  mutable fruitExists: bool;
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
let eFreightDeactivate = 202
let eLambdamanEatPill  = 300
let eLambdamanEatPowerPill  = 301
let eLambdamanEatFruit  = 302
let eEOL = 999999
;;

let schedule_tick world v = wl := TQ.add v !wl

let make field lambdaman_programs ghost_programs =
  if Array.length lambdaman_programs <= 0 || 2 < Array.length lambdaman_programs then
    failwith "# of lambdaman program is wrong";

  if Array.length ghost_programs <= 0 || 4 < Array.length ghost_programs then
    failwith "# of ghost program is wrong";

  let lambdaman_index = ref 0
  and ghost_index = ref 0 in

  let lambdamans = ref []
  and ghosts = ref [] in

  Array.iteri ( fun y line ->
    Array.iteri (fun x cell ->
      match cell with
      | Field.CLambdaManStart ->
         let t = Lambdaman.make !lambdaman_index x y lambdaman_programs.(!lambdaman_index) in
         lambdamans := t :: !lambdamans;
         incr lambdaman_index
      | Field.CGhostStart ->
         let t = Ghost.make !ghost_index x y ghost_programs.(!ghost_index mod (Array.length ghost_programs)) in
         ghosts := t :: !ghosts;
         incr ghost_index
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
    fruitExists = false;
    wl = !wl;
  }
;;

let next_tick world = 
  let (tick, event_id, event_arg) = min_elt !world.wl in
  match event_id with
  | eFruitAppear ->
      world.fruitExists <- true;
  | eFruitDisappear ->
      world.fruitExists <- false;
  | eEOL ->
      raise Exit (* FIXME *)
  | eLambdamanMove ->
      let lambdaman = world.lambdaman.(event_arg) in
      (* FIXME: run program *)
      (* FIXME: move lambdaman *)
      let is_eating = begin match world.field.(lambdaman.Lambdaman.y).(lambdaman.Lambdaman.x) with
      | Field.CPill ->
          schedule_tick world (tick, eLambdamanEatPill, event_arg);
          true
      | Field.CPowerPill -> (* Eat Power Pill *)
          schedule_tick world (tick, eLambdamanEatPowerPill, event_arg);
          true
      | Field.CFruitLocation -> 
          schedule_tick world (tick, eLambdamanEatFruit, event_arg);
          world.fruitsExist
      | _ -> false
      end;
      schedule_tick world ((tick + tick_move_lambdaman is_eating), eLambdamanMove, event_arg)
  | eGhostMove ->
      let ghost = world.ghost.(event_arg) in
      (* FIXME: run program *)
      (* FIXME: move lambdaman *)
      schedule_tick world ((tick + tick_move_ghost ghost), eGhostMove, event_arg)
  | eLambdamanEatPill ->
      let lambdaman = world.lambdaman.(event_arg) in
      lambdaman.score <- !lambdaman.score + score_pill;
      (* FIXME: count remaining pills and schedule win event if 0 *)
      world.field.(lambdaman.Lambdaman.y).(lambdaman.Lambdaman.x) <- Field.Empty
  | eLambdamanEatPowerPill ->
      let lambdaman = world.lambdaman.(event_arg) in
      lambdaman.score <- !lambdaman.score + score_power_pill;
      (* FIXME: activate fright mode *)
      world.field.(lambdaman.Lambdaman.y).(lambdaman.Lambdaman.x) <- Field.Empty
  | eLambdamanEatFruit ->
      if !world.fruitsExist then
        world.fruitsExist <- false;
        let lambdaman = world.lambdaman.(event_arg) in
        lambdaman.score <- !lambdaman.score + score_fruit world.field


  | eFreightDeactivate

(*
let tick tick_id t =
  move_lambdamans_if_necessary tick_id t;
  move_ghosts_if_necessary tick_id t;
  check_fright_mode_deactivating tick_id t;
  check_fruit_appearing tick_id t;
*)



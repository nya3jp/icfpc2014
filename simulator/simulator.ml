(* simulator code will be here. *)

(* simulator state *)
type t = {
  lambdamans: Lambdaman.t array;
  ghosts:     Ghost.t array;
  field:      Field.t;
  mutable fruitExists: bool;
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

  {
    lambdamans = Array.of_list (List.rev !lambdamans);
    ghosts = Array.of_list (List.rev !ghosts);
    field = field;
    fruitExists = false;
  }
;;

(*---- SCORE TABLE ----*)
(*
let score_pill = 10
let score_power_pill = 50
let score_fruit field = match level_of_field 
*)

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



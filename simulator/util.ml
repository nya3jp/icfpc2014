type direction =
  | Up
  | Right
  | Down
  | Left

let int_of_direction = function
  | Up -> 0
  | Right -> 1
  | Down -> 2
  | Left -> 3

let direction_of_int = function
  | 0 -> Up
  | 1 -> Right
  | 2 -> Down
  | 3 -> Left
  | x -> failwith ("invalid direction: " ^ string_of_int x)

let reverse_direction = function
  | Up -> Down
  | Right -> Left
  | Down -> Up
  | Left -> Right
;;


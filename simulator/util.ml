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

let reverse_direction = function
  | Up -> Down
  | Right -> Left
  | Down -> Up
  | Left -> Right
;;


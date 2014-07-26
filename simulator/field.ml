type cell =
  | CWall
  | CEmpty
  | CPill
  | CPowerPill
  | CFruitLocation
  | CLambdaManStart
  | CGhostStart

type t = cell array array

let get t ~y ~x =
  t.(y).(x)

let int_of_cell  = function
  | CWall -> 0
  | CEmpty -> 1
  | CPill -> 2
  | CPowerPill -> 3
  | CFruitLocation -> 4
  | CLambdaManStart -> 5
  | CGhostStart -> 6

let string_of_cell = function
  | CWall -> "#"
  | CEmpty -> " "
  | CPill -> "."
  | CPowerPill -> "o"
  | CFruitLocation -> "%"
  | CLambdaManStart -> "\\"
  | CGhostStart -> "="

let cell_of_char = function
  | '#' -> CWall
  | ' ' -> CEmpty
  | '.' -> CPill
  | 'o' -> CPowerPill
  | '%' -> CFruitLocation
  | '\\' -> CLambdaManStart
  | '=' -> CGhostStart
  | x -> failwith ("Illegal cell character: " ^ (Char.escaped x))

let string_of_field_line l = Array.fold_left (fun str c -> str ^ (string_of_cell c)) "" l

let string_of_field m = Array.fold_left (fun str line -> str ^ (string_of_field_line line) ^ "\n") "" m

let encode_field _ = ()

let width_of_field field = Array.length field.(0)
let height_of_field field = Array.length field
let level_of_field field = ((width_of_field field) * (height_of_field field) - 1) / 100 + 1

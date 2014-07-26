type cell =
| CWall
| CEmpty
| CPill
| CPowerPill
| CFruitLocation
| CLambdaManStart
| CGhostStart

type map = cell array array

let encode_cell = function
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

let string_of_map_line l = Array.fold_left (fun str c -> str ^ (string_of_cell c)) "" l

let string_of_map m = Array.fold_left (fun str line -> str ^ (string_of_map_line line) ^ "\n") "" m

type world = { map: map }

let encode_map _ = ()

let encode_world world =
  encode_map (world.map)

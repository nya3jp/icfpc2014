type map = string array

let string_of_map m = Array.fold_left (fun str line -> str ^ line ^ "\n") "" m

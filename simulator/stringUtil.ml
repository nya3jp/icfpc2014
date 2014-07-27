let remove_comment line =
  try
    let pos = String.index line ';' in
    String.sub line 0 pos
  with
  | Not_found -> line
;;

let trim str =
  let len = String.length str in
  let left = ref 0
  and right = ref (len - 1) in
  while !left < len && str.[!left] = ' ' do
    incr left
  done;
  while 0 <= !right && str.[!right] = ' ' do
    decr right
  done;
  if !left < !right then
    String.sub str !left (!right - !left + 1)
  else
    ""
;;

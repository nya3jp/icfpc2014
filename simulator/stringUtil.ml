let remove_comment line =
  try
    let pos = String.index line ';' in
    String.sub line 0 pos
  with
  | Not_found -> line
;;

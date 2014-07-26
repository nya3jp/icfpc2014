open StringUtil

(*
let parse line : Ghost.instruction =

;;
*)

let read filname : Ghost.ginstruction array =
  failwith "not implemented yet"

(*
  let in_chan = open_in filename in
  let result = ref [] in

  try
    while true do
      let line = String.trim (remove_comment (input_line in_chan)) in
      if line != "" then
        let instr = parse line in
        result := instr :: !result
      done
  with
    End_of_file ->
      close_in in_chan;
      Array.of_list (List.rev !result)
;;



*)

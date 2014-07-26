open StringUtil

let read filename : Ghost.ginstruction array =
  let in_chan = open_in filename in
  let result = ref [] in
  try
    while true do
      let line = String.trim (remove_comment (input_line in_chan)) in
      if line != "" then begin
        let instr = GhostAiParser.instruction GhostAiLexer.token (Lexing.from_string line) in
        result := instr :: !result
      end
    done;
    failwith "Shouldn't come here."
  with
    End_of_file ->
      close_in in_chan;
      Array.of_list (List.rev !result)

(*
let parse line : Ghost.instruction =

;;
*)


(*
;;



*)

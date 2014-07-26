open StringUtil

let read filename : Ghost.ginstruction array =
  let in_chan = open_in filename in
  let result = ref [] in
  try
    let linenumber = ref 0 in
    while true do
      let line = String.trim (remove_comment (input_line in_chan)) in
      incr linenumber;
      if line <> "" then begin
        try
          let instr = GhostAiParser.instruction GhostAiLexer.token (Lexing.from_string line) in
          result := instr :: !result
        with e ->
          Printf.printf "Parse Error in File %s Line %d: [%s]\n" filename !linenumber line;
          raise e
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

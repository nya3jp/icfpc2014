open StringUtil

let read filename =
  let in_chan = open_in filename in
  let result = ref [] in
  try
    let line_number = ref 0 in
    while true do
      let line = StringUtil.trim (remove_comment (input_line in_chan)) in
      incr line_number;
      if line <> "" then begin
        try
          let instr = LambdamanParser.instruction LambdamanLexer.token (Lexing.from_string line) in
          result := instr :: !result
        with e ->
          Printf.printf "Parse Error in File %s Line %d: [%s]\n" filename !line_number line;
          raise e
      end
    done;
    failwith "Shouldn't come here."
  with
    End_of_file ->
      close_in in_chan;
      Array.of_list (List.rev !result)


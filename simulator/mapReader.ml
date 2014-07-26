let read filename : World.map =
  let in_chan = open_in filename in
  let result = ref [] in
  try
    while true do
      let line_string = String.trim (input_line in_chan) in
      let line = ref [] in
      String.iter (fun c -> line := (World.cell_of_char c) :: !line) line_string;
      result := (Array.of_list (List.rev !line)) :: !result
    done;
    failwith "Shouldn't come here."
  with
    End_of_file ->
      close_in in_chan;
      Array.of_list (List.rev !result)

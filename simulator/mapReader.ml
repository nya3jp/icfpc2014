let read filename : World.map =
  let in_chan = open_in filename in
  let result = ref [] in
  try
    while true do
      let line = String.trim (input_line in_chan) in
      result := line :: !result
    done;
    failwith "Shouldn't come here."
  with
    End_of_file ->
      close_in in_chan;
      Array.of_list (List.rev !result)

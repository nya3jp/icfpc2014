let parse_ghosts str =
  let rec iter ghosts str =
    if str = "" then
      Array.of_list (List.rev ghosts)
    else
      try
        let pos = String.index str ',' in
        let ghost = String.sub str 0 pos in
        iter (ghost :: ghosts) (String.sub str (pos + 1) (String.length str - pos - 1))
      with
      | Not_found -> Array.of_list (List.rev (str :: ghosts))
  in
  iter [] str
;;

(* *)
let main () =
  Printexc.record_backtrace true;

  let pos = ref 1 in
  begin
    try
      while !pos < Array.length Sys.argv do
        if Sys.argv.(!pos) = "-v" then begin
          Simulator.conf_quiet := false;
          print_endline "verbose mode";
          incr pos
        end else if Sys.argv.(!pos) = "-e" then begin
          Simulator.conf_eternal := true;
          print_endline "eternal mode";
          incr pos
        end else
          raise Exit
      done;
    with
    | Exit -> ()
  end;

  if Array.length Sys.argv <> !pos + 3 then
    failwith "usage: ./simulator [options] map ai ghost-ai";

  let fieldName = Sys.argv.(!pos) in
  let lambdamanFilename = Sys.argv.(!pos + 1) in
  let ghostFilenames = Sys.argv.(!pos + 2) in

  let field = FieldReader.read fieldName in
  let ai = LambdamanReader.read lambdamanFilename in
  let ghosts = Array.map (fun ghostFilename ->
    GhostAiReader.read ghostFilename
  ) (parse_ghosts ghostFilenames) in

  (* print_string (Field.string_of_field field); *)

  let simulator = Simulator.make field [|ai|] ghosts in
  Simulator.run simulator
;;

let _ =
  if not !Sys.interactive then main ()

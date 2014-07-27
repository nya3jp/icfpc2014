let parse_ghosts ghosts =
  let list = Str.split (Str.regexp ",") ghosts in
  Array.of_list list
;;

(* *)
let main () =
  Printexc.record_backtrace true;
  if Array.length Sys.argv <> 4 then
    failwith "usage: ./simulator map ai ghost-ai";

  let fieldName = Sys.argv.(1) in
  let lambdamanFilename = Sys.argv.(2) in
  let ghostFilenames = Sys.argv.(3) in

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

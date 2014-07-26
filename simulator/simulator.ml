(* *)
let main () =
  Printexc.record_backtrace true;
  if Array.length Sys.argv <> 4 then
    failwith "usage: ./simulator map ai ghost-ai";

  let mapName = Sys.argv.(1) in
  let lambdamanFilename = Sys.argv.(2) in
  let ghostFilename = Sys.argv.(3) in

  let map = MapReader.read mapName in
(*  let ai = LambdamanReader.read lambdamanFilename in *)
  let ghost = GhostAiReader.read ghostFilename in

  print_string (World.string_of_map map);

  failwith "not implemented yet"
;;

let _ =
  if not !Sys.interactive then main ()

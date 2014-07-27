let run_program () =
  let name = Sys.argv.(1) in
  let program = LambdamanReader.read name in
  Lambdaman.eval_program program
;;

(* *)
let main () =
  Printexc.record_backtrace true;
  if Array.length Sys.argv <> 4 then
    failwith "usage: ./simulator map ai ghost-ai";

  let fieldName = Sys.argv.(1) in
  let lambdamanFilename = Sys.argv.(2) in
  let ghostFilename = Sys.argv.(3) in

  let field = FieldReader.read fieldName in
  let ai = LambdamanReader.read lambdamanFilename in
  let ghost = GhostAiReader.read ghostFilename in

  print_string (Field.string_of_field field);

  let simulator = Simulator.make field [|ai|] [|ghost|] in
  Simulator.run simulator
;;

let _ =
  if not !Sys.interactive then main ()

(*
let _ =
  run_program ()
*)
;;

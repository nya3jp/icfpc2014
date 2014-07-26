(* *)
let main () =
  if Array.length Sys.argv <> 3 then
    failwith "usage: ./simulator map ai ghost-ai";

  let fieldName = Sys.argv.(1) in
  let lambdamanFilename = Sys.argv.(2) in
  let ghostFilename = Sys.argv.(3) in

  let field = FieldReader.read fieldName in
  let ai = LambdamanReader.read lambdamanFilename in
  let ghost = GhostAiReader.read ghostFilename in

  failwith "not implemented yet"
;;

let _ =
  if not !Sys.interactive then main ()

let run_program () =
  let name = Sys.argv.(1) in
  let program = LambdamanReader.read name in
  Lambdaman.eval_program program
;;

let _ =
  run_program ()
;;


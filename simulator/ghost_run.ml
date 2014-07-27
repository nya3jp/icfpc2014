(* run only ghost program *)

let syscallback n env =
  Printf.printf "syscall is not implemented for ghost_run (%d)\n" n
;;

let run_program () =
  let name = Sys.argv.(1) in
  let program = GhostAiReader.read name in
  let t = Ghost.make 0 8 15 program in
  let d = Ghost.eval t syscallback in
  print_endline (string_of_int d)
;;

let _ =
  run_program ()
;;




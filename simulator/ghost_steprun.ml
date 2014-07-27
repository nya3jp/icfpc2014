(* run only ghost program *)

open Ghost

let syscallback n env =
  Printf.printf "syscall is not implemented for ghost_run (%d)\n" n
;;

let evalstep (t : Ghost.t) syscallback =
  try
    eval_ginstruction t.env syscallback t.program.(t.env.pc);
    dump t;
  with
  | Halt_exception -> ()
;;

let run_program () =
  let name = Sys.argv.(1) in
  let program = GhostAiReader.read name in
  let t = Ghost.make 0 30 30 program in
  dump t;
  while true do
    print_string "Press enter to next step";
    ignore(read_line ());
    evalstep t syscallback;
  done;
;;

let _ =
  run_program ()
;;




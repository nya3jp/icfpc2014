(* run only ghost program *)

open Ghost

let syscallback n env =
  Printf.printf "syscall is not implemented for ghost_run (%d)\n" n
;;

let evalstep (t : Ghost.t) syscallback =
  eval_ginstruction t.env syscallback t.program.(t.env.pc);
;;

let run_program () =
  let name = Sys.argv.(1) in
  let program = GhostAiReader.read name in
  let t = Ghost.make 0 30 30 program in

  while true do
    try
      while true do
        print_endline (string_of_instruction t.program.(t.env.pc));
        dump t;
        print_string "Press enter to next step";
        ignore(read_line ());
        evalstep t syscallback;
      done;
    with
    | Halt_exception ->
       print_endline "HALTed. Rerun with the current state.";
       t.env.pc <- 0
  done
;;

let _ =
  run_program ()
;;




(* *)
let main () =
  if Array.length Sys.argv <> 6 then
    failwith "usage: ./simulator map ai ghost-ai ghost-ai ghost-ai ghost-ai";
;;

let _ = main ()

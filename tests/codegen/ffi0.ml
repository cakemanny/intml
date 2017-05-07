(* write to the screen *)

external write : int -> string -> int = "ml__write"

let main () =
  if write 1 "hello\n" < 0
  then 1
  else 0


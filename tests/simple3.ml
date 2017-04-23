
(* test codegen for BIND_EXPR *)
let main () =
  let g z w = z * z * w in (*  let g = fun z -> fun w -> z * z * w  *)
  if 1 then
    let x = 1 + 2 in
    g 5 x (* 25 * 3 = 75 *)
  else
    let y = 6 in
    let x = 1 + 2 in
    g y x



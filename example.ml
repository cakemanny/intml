
type intop = int -> int -> int
type intfunc = int -> int

let f (x : int) = x * 49

let y x =
    let f y z = y * z in
    let f y z = y * z in
    f x x

(* these should be the same as *)
let app1 = 2 + f 1
let app2 = 2 + (f 1)
let app3 =
  let fx (z:int) (w:int) = z + w in
  let g = fx in
  1 + 2 + fx 2 2 * g 3 3

let infer (x:int) = x         (* want this to now gain the type int -> int *)
let deduce = infer 10

let main () =
    f 2 + f 25



type intop = int -> int -> int
type intfunc = int -> int

let f (x : int -> int) = x  x

let y x =
    let f y z = y * z in
    let f y z = y * z in
    f x x

(* these should be the same as *)
let app1 = 2 + f 1
let app1 = 2 + (f 1)

let main () =
    f 2 + f 25


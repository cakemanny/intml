
let main () =
  (*let f x y = x * y in*)
  let f x = let f2 y = x * y in f2 in
  f 5 6


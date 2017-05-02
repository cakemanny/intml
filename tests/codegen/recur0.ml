let sum =
  let rec go acc xs =
    let hd::tail = xs in
    if hd then
      go (hd + acc) tail
    else
      acc
  in go 0

let main () =
  sum [ 4; 3; 2; 1; 0; 0 ]


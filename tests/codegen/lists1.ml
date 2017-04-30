(* do some pattern patching *)
let main () =
  let head xs =
    let x :: _ = xs
    in x
  in head [ 4; 3; 2; 1 ]



(* some more pattern patching *)
let main () =
  let snd xs =
    let _ :: x :: _ = xs
    in x
  in snd [ 4; 3; 2; 1 ]




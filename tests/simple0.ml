
(* A very easy first target for code generator *)

let main () =
  42

(*
int ml__main()
{
    return 42;
}

int main()
{
    return ml__main();
}
*)


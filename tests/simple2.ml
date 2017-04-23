
let x = 5

let main () =
  x + x

(* Would look something like:

int ml__main(ml__main__closure* cl)
{
  return cl->x * cl->x;
}
int ml__x__init() { return 5; } /* annotate init since it's a let not a func */
int main()
{
  ml_main_closure* cl = new ml_main_closure();
  cl->x = ml__x__init();
  return ml_main(cl);
}

*)


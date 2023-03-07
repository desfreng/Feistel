open Feistel.BitSet

let a = from_string "11101011101010110011"
let b = a <<> 1
let c = a <<> 10
let d = a <>> 1
let e = a <>> 10
let f = !a

let print_list list =
  let _ =
    print_newline ();
    List.fold_left
      (fun acc a ->
        Printf.printf "%i '%s' = %i\n" acc (to_string a) (to_int a);
        Stdlib.( + ) acc 1)
      0 list
  in
  ()

let () = print_list [ a; b; c; d; e; f ]

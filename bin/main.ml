open Feistel

let a = BitSet.from_string "00001011101010110011"
let b = BitSet.to_block 10 a
let c = BitSet.from_block b
let d = BitSet.to_bool_list a

let () =
  print_newline ();
  Printf.printf "a : '%s' = %i\n" (BitSet.to_string a) (BitSet.to_int a);
  for i = 0 to List.length b - 1 do
    let tmp = List.nth b i in
    Printf.printf "a[%i] : '%s' = %i\n" i (BitSet.to_string tmp)
      (BitSet.to_int tmp)
  done;
  Printf.printf "c : '%s' = %i\n" (BitSet.to_string c) (BitSet.to_int c);
  Printf.printf "%s\n\n"
    (List.fold_left
       (fun acc elm ->
         if elm then
           if acc = "" then
             "1"
           else
             acc ^ "; 1"
         else if acc = "" then
           "0"
         else
           acc ^ "; 0")
       "" d)

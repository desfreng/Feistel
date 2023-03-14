open Feistel
open Feistel.Padding

let () =
  Printexc.record_backtrace true;
  Printf.printf "\n\n\n"

module PAD_SC = PKCS7

let a = BitSet.from_string "11001010"
let b1 = PAD_SC.pad a 2
let b2 = PAD_SC.pad a 3
let b3 = PAD_SC.pad a 4
let b4 = PAD_SC.pad a 5

let () =
  Printf.printf "                                %s\n" BitSet.(to_string a);
  Printf.printf "                        %s\n" BitSet.(to_string b1);
  Printf.printf "                %s\n" BitSet.(to_string b2);
  Printf.printf "        %s\n" BitSet.(to_string b3);
  Printf.printf "%s\n" BitSet.(to_string b4);
  Printf.printf "\n";
  Printf.printf "%s\n" BitSet.(to_string (PAD_SC.unpad b1));
  Printf.printf "%s\n" BitSet.(to_string (PAD_SC.unpad b2));
  Printf.printf "%s\n" BitSet.(to_string (PAD_SC.unpad b3));
  Printf.printf "%s\n" BitSet.(to_string (PAD_SC.unpad b4));
  Printf.printf "\n"

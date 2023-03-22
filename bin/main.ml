open Feistel

let () =
  Printexc.record_backtrace true;
  Printf.printf "\n\n\n"

module CIFR = Cipher.Lucifer

let k =
  CIFR.build_key
    (BitSet.from_string
       "01010011001010100000011111010001101111000110100011100000000110101011100010010011111010100111010010100101001110111010100100000010")

let msg1 =
  BitSet.from_string
    "10101010101010100101010101010101101010101010101001010101010101011010101010101010010101010101010110101010101010100101010101010101"

let msg2 =
  BitSet.from_string
    "00111000001111000111110001100110110001101100001110000000100000011000000100000001110000110110001101100110001111100011110000011100"

let c1 = CIFR.encrypt k msg1
let c2 = CIFR.encrypt k msg2
let d1 = CIFR.decrypt k c1
let d2 = CIFR.decrypt k c2

let () =
  Printf.printf "\n";
  Printf.printf "msg1  : %s \ncipher1  : %s\n\n" (BitSet.to_string msg1)
    (BitSet.to_string c1);
  Printf.printf "msg2  : %s \ncipher2  : %s\n\n" (BitSet.to_string msg2)
    (BitSet.to_string c2);
  Printf.printf "Status : %s\n"
    (if
       BitSet.to_string d1 = BitSet.to_string msg1
       && BitSet.to_string d2 = BitSet.to_string msg2
     then
       "OK"
     else
       "NOK");
  Printf.printf "\n"

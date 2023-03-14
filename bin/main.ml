open Feistel

let () =
  Printexc.record_backtrace true;
  Printf.printf "\n\n\n"

module CIFR = Cipher.Lucifer
module T = CipherSuite.MakeSuite (CIFR) (OperationMode.CTR) (Padding.PKCS7)

let key = T.build_key (BitSet.from_int64 0xFF00FF00L 128)
let msg = BitSet.from_string "1000000011011100"
let iv = BitSet.(from_int 10000058 (CIFR.block_size / 2))
let cipher = T.encrypt iv msg key
let decipher = T.decrypt iv cipher key

let () =
  Printf.printf "\n";
  Printf.printf "msg      : %s\n" (BitSet.to_string msg);
  Printf.printf "iv       : %s\n" (BitSet.to_string iv);
  Printf.printf "cipher   : %s\n" (BitSet.to_string cipher);
  Printf.printf "decipher : %s\n" (BitSet.to_string decipher);
  Printf.printf "\n\nOk : %s"
    (if BitSet.to_int decipher = BitSet.to_int msg then
      "TRUE"
    else
      "FALSE");
  Printf.printf "\n"

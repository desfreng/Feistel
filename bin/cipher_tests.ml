open Feistel
open Feistel.Cipher

let bitkey = BitSet.(from_int64 1L 64 ^ from_int64 1L 64)
let key = Lucifer.to_key bitkey
let plaintext = BitSet.from_int 0xFFF0000FFFF 128
let ciphertext = Lucifer.encrypt key plaintext
let dectext = Lucifer.decrypt key ciphertext

let rec enc_loop acc i =
  if i = 0 then
    acc
  else
    enc_loop (Lucifer.encrypt key acc) (i - 1)

let rec dec_loop acc i =
  if i = 0 then
    acc
  else
    dec_loop (Lucifer.decrypt key acc) (i - 1)

let ciphertext_16 = enc_loop plaintext 16
let ciphertext_32 = enc_loop plaintext 32
let dectext_16 = dec_loop ciphertext_16 16
let dectext_32 = dec_loop ciphertext_32 32

let () =
  Printf.printf "\n";
  Printf.printf "Key          : %s\n" (BitSet.to_string bitkey);
  Printf.printf "PlainText    : %s\n" (BitSet.to_string plaintext);
  Printf.printf "\n";
  Printf.printf "DecText      : %s\n" (BitSet.to_string dectext);
  Printf.printf "DecText16    : %s\n" (BitSet.to_string dectext_16);
  Printf.printf "DecText32    : %s\n" (BitSet.to_string dectext_32);
  Printf.printf "\n";
  Printf.printf "CipherText   : %s\n" (BitSet.to_string ciphertext);
  Printf.printf "CipherText16 : %s\n" (BitSet.to_string ciphertext_16);
  Printf.printf "CipherText32 : %s\n" (BitSet.to_string ciphertext_32);
  Printf.printf "\n"

let bitkey = BitSet.from_string "1010000010"
let key = SDES.to_key bitkey
let plaintext = BitSet.from_int 0x55 8
let ciphertext = SDES.encrypt key plaintext
let deciphertext = SDES.decrypt key ciphertext

let () =
  Printf.printf "\n";
  Printf.printf "Key          : %s\n" (BitSet.to_string bitkey);
  Printf.printf "PlainText    : %s\n" (BitSet.to_string plaintext);
  Printf.printf "DecText      : %s\n" (BitSet.to_string deciphertext);
  Printf.printf "CipherText   : %s\n" (BitSet.to_string ciphertext);
  Printf.printf "\n"

let bitkey =
  BitSet.(
    from_string
      "011000010100101010100000110101010010010101100100100101010001010010110011011111100111001001010100")

let key = SimpleSP96.to_key bitkey
let plaintext = BitSet.from_int 0xFFF0000FFFF 96
let ciphertext = SimpleSP96.encrypt key plaintext
let dectext = SimpleSP96.decrypt key ciphertext

let rec enc_loop acc i =
  if i = 0 then
    acc
  else
    enc_loop (SimpleSP96.encrypt key acc) (i - 1)

let rec dec_loop acc i =
  if i = 0 then
    acc
  else
    dec_loop (SimpleSP96.decrypt key acc) (i - 1)

let ciphertext_16 = enc_loop plaintext 16
let ciphertext_32 = enc_loop plaintext 32
let dectext_16 = dec_loop ciphertext_16 16
let dectext_32 = dec_loop ciphertext_32 32

let () =
  Printf.printf "\n";
  Printf.printf "Key          : %s\n" (BitSet.to_string bitkey);
  Printf.printf "PlainText    : %s\n" (BitSet.to_string plaintext);
  Printf.printf "\n";
  Printf.printf "DecText      : %s\n" (BitSet.to_string dectext);
  Printf.printf "DecText16    : %s\n" (BitSet.to_string dectext_16);
  Printf.printf "DecText32    : %s\n" (BitSet.to_string dectext_32);
  Printf.printf "\n";
  Printf.printf "CipherText   : %s\n" (BitSet.to_string ciphertext);
  Printf.printf "CipherText16 : %s\n" (BitSet.to_string ciphertext_16);
  Printf.printf "CipherText32 : %s\n" (BitSet.to_string ciphertext_32);
  Printf.printf "\n"

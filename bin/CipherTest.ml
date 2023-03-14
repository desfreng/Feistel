open Feistel
open Feistel.Cipher
module CIFR = SDES

let bitkey = BitSet.from_string "1100110011"
let key = CIFR.build_key bitkey
let plaintext = BitSet.from_int 71 8
let ciphertext = CIFR.encrypt key plaintext
let dectext = CIFR.decrypt key ciphertext

let rec enc_loop acc i =
  if i = 0 then
    acc
  else
    enc_loop (CIFR.encrypt key acc) (i - 1)

let rec dec_loop acc i =
  if i = 0 then
    acc
  else
    dec_loop (CIFR.decrypt key acc) (i - 1)

let ciphertext_16 = enc_loop plaintext 16
let ciphertext_32 = enc_loop plaintext 32
let dectext_16 = dec_loop ciphertext_16 16
let dectext_32 = dec_loop ciphertext_32 32

let a () =
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

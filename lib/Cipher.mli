module type t = sig
  type key
  (** Key type *)

  val build_key : BitSet.t -> key
  (** Return the key encoded by provided bitset *)

  val encrypt : key -> BitSet.t -> BitSet.t
  (** Transform plaintext to ciphertext *)

  val decrypt : key -> BitSet.t -> BitSet.t
  (** Transform ciphertext to plaintext *)

  val block_size : int
  (** Block Size *)
end

(** Build a Cipher according given Feistel Network *)
module MakeFromNetwork (Net : Network.t) : t with type key = Net.key

module Lucifer : t
(** Lucifer Cipher *)

module SDES : t
(** Simple DES Cipher *)

module SimpleSP96 : t
(** A Simple Substitution-Permutation Feistel Cipher *)

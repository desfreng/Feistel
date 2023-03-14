module type t = sig
  type key
  (** Key type *)

  val to_key : BitSet.t -> key
  (** Return the key encoded by provided bitset *)

  val encrypt : key -> BitSet.t -> BitSet.t
  (** Transform plaintext to ciphertext *)

  val decrypt : key -> BitSet.t -> BitSet.t
  (** Transform ciphertext to plaintext *)
end

module Lucifer : t
module SDES : t
module SimpleSP96 : t

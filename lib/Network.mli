module type t = sig
  type key
  (** Type of the master key*)

  type round_key
  (** Type of the round keys *)

  val begin_enc_function : BitSet.t -> BitSet.t
  (** Begin Encryption Function of the Network *)

  val end_enc_function : BitSet.t -> BitSet.t
  (** End Function of the Network *)

  val begin_dec_function : BitSet.t -> BitSet.t
  (** Begin Decryption Function of the Network *)

  val end_dec_function : BitSet.t -> BitSet.t
  (** End Decryption of the Network *)

  val number_of_round : int
  (** Number of round of this network *)

  val to_key : BitSet.t -> key
  (** Return the key encoded by provided bitset *)

  val gen_round_key : key -> int -> round_key
  (** [gen_round_key master_key round_index] : Return the round key generated by the [master_key] for the round [round_index] *)

  val lsb_part_size : int
  (** Size of the least significant bits part of data (Right part) *)

  val msb_part_size : int
  (** Size of the most significant bits part of data (Left part) *)

  val round_function : round_key -> BitSet.t -> BitSet.t
  (** Round Function of the Network *)
end

(** Build a Cipher according given Feistel Network *)
module Make (Net : t) : Cipher.t with type key = Net.key

module Lucifer : t
module SDES : t
module SimpleSP96 : t

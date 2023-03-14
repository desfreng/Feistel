module type t = sig
  type key
  (** Key type *)

  val build_key : BitSet.t -> key
  (** Build a key from provided bitset *)

  val encrypt : key -> BitSet.t -> BitSet.t
  (** Transform plaintext to ciphertext *)

  val decrypt : key -> BitSet.t -> BitSet.t
  (** Transform ciphertext to plaintext *)

  val block_size : int
  (** Block Size *)
end

(** Build a Cipher according given Feistel Network *)
module MakeFromNetwork (Net : Network.t) = struct
  type key = Net.key

  (** Return the key encoded by provided bitset *)
  let build_key = Net.build_key

  (** Data block size *)
  let block_size = Net.lsb_part_size + Net.msb_part_size

  (** [_split bitset] : split [bitset] in two part.
      
  The first one is the least significant bits part.
  The secound is the most significant bits part. *)
  let _enc_split bitset = BitSet.split bitset Net.lsb_part_size

  let _dec_split bitset = BitSet.split bitset Net.msb_part_size

  (** [_permute bitset boolean] : if [boolean] then split [bitset], swap and 
      concatenate the two part. Do nothing otherwise. *)
  let _permute bitset boolean =
    if boolean then
      let lsb, msb = BitSet.split bitset (block_size / 2) in
      BitSet.concatenate msb lsb
    else
      bitset

  let _check_data plaintext =
    if BitSet.size plaintext != block_size then
      invalid_arg ("`data` must be of size " ^ string_of_int block_size)

  (** [decrypt master_key bitset] : Decrypt [bitset] by appliying rounds of the Feistel Network with the key [master_key] on the [bitset]. *)
  let encrypt master_key plaintext =
    let rec _loop bitset round_id =
      match round_id with
      | i when i < 0 -> invalid_arg "`round_id` must be positive"
      | i when i >= Net.number_of_round -> bitset
      | i ->
          let round_key = Net.gen_round_key master_key i in
          let lsb_i, msb_i = _enc_split bitset in
          let lsb_ii, msb_ii =
            (BitSet.(msb_i lxor Net.round_function round_key lsb_i), lsb_i)
          in
          let round_bitset = BitSet.concatenate lsb_ii msb_ii in
          _loop round_bitset (round_id + 1)
    in
    _check_data plaintext;
    let _pre_encr = Net.begin_enc_function plaintext in
    let _after_encr = _loop _pre_encr 0 in
    Net.end_enc_function _after_encr

  (** [decrypt master_key bitset] : Decrypt [bitset] by reverse appliying rounds of the Feistel Network with the key [master_key] on the [bitset]. *)
  let decrypt master_key ciphertext =
    let rec _loop bitset round_id =
      match round_id with
      | i when i >= Net.number_of_round ->
          invalid_arg "`round_id` must be smaller than the number of round"
      | i when i <= -1 -> bitset
      | i ->
          let round_key = Net.gen_round_key master_key i in
          let lsb_ii, msb_ii = _dec_split bitset in
          let lsb_i, msb_i =
            (msb_ii, BitSet.(lsb_ii lxor Net.round_function round_key msb_ii))
          in
          let round_bitset = BitSet.concatenate lsb_i msb_i in
          _loop round_bitset (round_id - 1)
    in
    _check_data ciphertext;
    let _pre_decr = Net.begin_dec_function ciphertext in
    let _after_decr = _loop _pre_decr (Net.number_of_round - 1) in
    Net.end_dec_function _after_decr
end

module Lucifer = MakeFromNetwork (Network.Lucifer)
(** Lucifer Cipher *)

module SDES = MakeFromNetwork (Network.SDES)
(** Simple DES Cipher *)

module SimpleSP96 = MakeFromNetwork (Network.SimpleSP96)
(** A Simple Substitution-Permutation Feistel Cipher *)

module type t = sig
  type key

  val encrypt : BitSet.t -> BitSet.t -> key -> BitSet.t
  val decrypt : BitSet.t -> BitSet.t -> key -> BitSet.t
  val build_key : BitSet.t -> key
end

(** Make a CipherSuite from a [Cipher], a [Mode] of operation, and a [Padding] scheme *)
module MakeSuite
    (C : Cipher.t) (OM : functor (C : Cipher.t) ->
      OperationMode.t with type key = C.key)
    (P : Padding.t) =
struct
  type key = C.key

  module M = OM (C)

  (** Build a key from provided bitset *)
  let build_key = C.build_key

  let pad_mult =
    if P.byte_padding then
      C.block_size / 8
    else
      C.block_size

  (** [encrypt iv plaintext key] : Encrypt [plaintext] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let encrypt iv plaintext key =
    let padded_bitset = P.pad plaintext pad_mult in
    let padded_plaintext_blocks = BitSet.to_block C.block_size padded_bitset in
    let padded_cipher_blocks = M.encrypt_blocs iv key padded_plaintext_blocks in
    BitSet.from_block padded_cipher_blocks

  (** [decrypt iv ciphertext key] : Decrypt [ciphertext] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let decrypt iv ciphertext key =
    if BitSet.size ciphertext mod C.block_size != 0 then
      invalid_arg
        ("Cipher text size must be a multiple of " ^ string_of_int C.block_size)
    else
      let ciphertext_blocks = BitSet.to_block C.block_size ciphertext in
      let plaintext_block = M.decrypt_blocs iv key ciphertext_blocks in
      let padded_plaintext = BitSet.from_block plaintext_block in
      P.unpad padded_plaintext
end

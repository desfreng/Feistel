module type t = sig
  type key
  (** Key Type *)

  val encrypt : BitSet.t -> BitSet.t -> key -> BitSet.t
  (** [encrypt iv plaintext key] : Encrypt [plaintext] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)

  val decrypt : BitSet.t -> BitSet.t -> key -> BitSet.t
  (** [decrypt iv ciphertext key] : Decrypt [ciphertext] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)

  val build_key : BitSet.t -> key
  (** Build a key from provided bitset *)
end

(** Make a CipherSuite from a [Cipher], a [OperationMode] of operation, and a [Padding] scheme *)
module MakeSuite
    (C : Cipher.t) (_ : functor (C : Cipher.t) ->
      OperationMode.t with type key = C.key)
    (_ : Padding.t) : t with type key = C.key

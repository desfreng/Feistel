module type t = sig
  type key
  (** Key Type *)

  val encrypt_blocs : BitSet.t -> key -> BitSet.t list -> BitSet.t list
  (** [encrypt_blocs iv key plaintext_blocs] : Encrypt each bloc of [plaintext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)

  val decrypt_blocs : BitSet.t -> key -> BitSet.t list -> BitSet.t list
  (** [decrypt_blocs iv key ciphertext_blocs] : Decrypt each bloc of [ciphertext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
end

(** Electronic Codebook Mode of Operation *)
module ECB (C : Cipher.t) : t with type key = C.key

(** Cipher Block Chaining Mode of Operation *)
module CBC (C : Cipher.t) : t with type key = C.key

(** Counter Mode of Operation *)
module CTR (C : Cipher.t) : t with type key = C.key

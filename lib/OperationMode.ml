module type t = sig
  type key

  val encrypt_blocs : BitSet.t -> key -> BitSet.t list -> BitSet.t list
  val decrypt_blocs : BitSet.t -> key -> BitSet.t list -> BitSet.t list
end

(** Electronic Codebook Mode of Operation *)
module ECB (C : Cipher.t) = struct
  type key = C.key

  (** [encrypt_blocs iv key plaintext_blocs] : Encrypt each bloc of [plaintext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let encrypt_blocs iv key plaintext_blocs =
    ignore iv;
    List.map (C.encrypt key) plaintext_blocs |> List.rev

  (** [decrypt_blocs iv key ciphertext_blocs] : Decrypt each bloc of [ciphertext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let decrypt_blocs iv key ciphertext_blocs =
    ignore iv;
    List.map (C.decrypt key) ciphertext_blocs |> List.rev
end

(** Cipher Block Chaining Mode of Operation *)
module CBC (C : Cipher.t) = struct
  type key = C.key

  (** [encrypt_blocs iv key plaintext_blocs] : Encrypt each bloc of [plaintext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let encrypt_blocs iv key plaintext_blocs =
    List.fold_left
      (fun (prev_cipher, cipher_blocs) plaintext ->
        let altered_plain = BitSet.(plaintext lxor prev_cipher) in
        let ciphertext = C.encrypt key altered_plain in
        (ciphertext, ciphertext :: cipher_blocs))
      (iv, []) plaintext_blocs
    |> snd |> List.rev

  (** [decrypt_blocs iv key ciphertext_blocs] : Decrypt each bloc of [ciphertext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let decrypt_blocs iv key ciphertext_blocs =
    List.fold_left
      (fun (prev_cipher, plaintext_blocs) ciphertext ->
        let altered_plain = C.decrypt key ciphertext in
        let plaintext = BitSet.(altered_plain lxor prev_cipher) in
        (ciphertext, plaintext :: plaintext_blocs))
      (iv, []) ciphertext_blocs
    |> snd |> List.rev
end

(** Counter Mode of Operation *)
module CTR (C : Cipher.t) = struct
  type key = C.key

  (** [encrypt_blocs iv key plaintext_blocs] : Encrypt each bloc of [plaintext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let encrypt_blocs iv key plaintext_blocs =
    let size_iv = BitSet.size iv in
    let max_index = Z.shift_left Z.one size_iv in
    List.fold_left
      (fun (block_index, cipher_blocs) plaintext ->
        let to_encrypt = BitSet.(concatenate iv (from_Z block_index size_iv)) in
        ( Z.rem (Z.add block_index Z.one) max_index,
          BitSet.(plaintext lxor C.encrypt key to_encrypt) :: cipher_blocs ))
      (Z.zero, []) plaintext_blocs
    |> snd |> List.rev

  (** [decrypt_blocs iv key ciphertext_blocs] : Decrypt each bloc of [ciphertext_blocs] with the key [key].
      The Initialisation Vector [iv] is used to initialise the [Mode] of operation *)
  let decrypt_blocs iv key ciphertext_blocs =
    let size_iv = BitSet.size iv in
    let max_index = Z.shift_left Z.one size_iv in

    List.fold_left
      (fun (block_index, cipher_blocs) ciphertext ->
        let to_encrypt = BitSet.(concatenate iv (from_Z block_index size_iv)) in
        ( Z.rem (Z.add block_index Z.one) max_index,
          BitSet.(ciphertext lxor C.encrypt key to_encrypt) :: cipher_blocs ))
      (Z.zero, []) ciphertext_blocs
    |> snd |> List.rev
end

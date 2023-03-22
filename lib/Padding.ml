module type t = sig
  val byte_padding : bool
  val pad : BitSet.t -> int -> BitSet.t
  val unpad : BitSet.t -> BitSet.t
end

let _is_padded bitset_size bit_mult = bitset_size mod bit_mult = 0
let _is_bitset_padded bitset bit_mult = _is_padded (BitSet.size bitset) bit_mult

module RFC_1321 : t = struct
  (** Padded scheme bit defined as in RFC1321. Add a [1] and 
      then [0] such as the size is a multiple of provided [bit_mult] *)

  (** [true] if padding scheme is a byte padding one. *)
  let byte_padding = false

  (* [pad bitset mult] : Return a [bitset] with a size such as [size bitset mod mult = 0] *)
  let pad bitset bit_mult =
    let new_bitset = BitSet.(concatenate bitset (from_int 1 1)) in
    if _is_bitset_padded new_bitset bit_mult then
      new_bitset
    else
      let zero_bit_to_add = bit_mult - (BitSet.size new_bitset mod bit_mult) in
      BitSet.(concatenate new_bitset (zeros zero_bit_to_add))

  (* [unpad bitset] : Return the original [bitset] *)
  let unpad bitset =
    let bitset_size = BitSet.size bitset in
    let rec _loop index =
      if BitSet.bit_value index bitset then
        BitSet.extract bitset 0 index
      else
        _loop (index - 1)
    in
    _loop (bitset_size - 1)
end

module ANSI_X923 : t = struct
  (** Padded scheme bytes defined as in RFC1321. Add [0x00] multiple time and at the
      end the number of bytes added such as the byte size is a multiple of provided [byte_mult] *)

  (** [true] if padding scheme is a byte padding one. *)
  let byte_padding = true

  (* [pad bitset byte_mult] : Return a [bitset] with a size such as [(8*size bitset) mod byte_mult = 0] *)
  let pad bitset byte_mult =
    let bitset_size = BitSet.size bitset in
    if bitset_size mod 8 != 0 then
      invalid_arg "`bitset` must be a multiple of 8"
    else
      let bit_mult = byte_mult * 8 in
      let bitset_to_add =
        if _is_padded (bitset_size + 8) bit_mult then
          BitSet.from_int 0x01 8
        else
          let non_end_bits_to_add =
            bit_mult - ((bitset_size + 8) mod bit_mult)
          in
          if non_end_bits_to_add >= 255 * 8 then
            invalid_arg "`byte_mult` is too large for ANSI X9.23"
          else
            BitSet.concatenate
              (BitSet.zeros non_end_bits_to_add)
              (BitSet.from_int ((non_end_bits_to_add / 8) + 1) 8)
      in
      BitSet.concatenate bitset bitset_to_add

  (* [unpad bitset] : Return the original [bitset] *)
  let unpad bitset =
    let bitset_size = BitSet.size bitset in
    if bitset_size mod 8 != 0 then
      invalid_arg "`bitset` must be a multiple of 8"
    else
      let bytes_size = bitset_size / 8 in
      let bits_to_remove = 8 * BitSet.byte_value (bytes_size - 1) bitset in
      let new_size = bitset_size - bits_to_remove in
      BitSet.extract bitset 0 new_size
end

module PKCS7 : t = struct
  (** Padded scheme bytes defined as in RFC1321. Add multiple time the number 
      of bytes added such as the byte size is a multiple of provided [byte_mult] *)

  (** [true] if padding scheme is a byte padding one. *)
  let byte_padding = true

  (* [pad bitset byte_mult] : Return a [bitset] with a size such as [(8*size bitset) mod byte_mult = 0] *)
  let pad bitset byte_mult =
    let bitset_size = BitSet.size bitset in
    if bitset_size mod 8 != 0 then
      invalid_arg "`bitset` must be a multiple of 8"
    else
      let bit_mult = byte_mult * 8 in
      let bitset_to_add =
        if _is_padded (bitset_size + 8) bit_mult then
          BitSet.from_int 0x01 8
        else
          let byte_to_add = bit_mult - (bitset_size mod bit_mult) in
          if byte_to_add >= 255 then
            invalid_arg "`byte_mult` is too large for PKCS#7"
          else
            BitSet.repeat_byte (byte_to_add / 8) (byte_to_add / 8)
      in
      BitSet.concatenate bitset bitset_to_add

  (* [unpad bitset] : Return the original [bitset] *)
  let unpad bitset =
    let bitset_size = BitSet.size bitset in
    if bitset_size mod 8 != 0 then
      invalid_arg "`bitset` must be a multiple of 8"
    else
      let bytes_size = bitset_size / 8 in
      let bits_to_remove = 8 * BitSet.byte_value (bytes_size - 1) bitset in
      let new_size = bitset_size - bits_to_remove in
      BitSet.extract bitset 0 new_size
end

module Zero : t = struct
  (** Padded scheme bytes defined as adding [0] bit such as the size is a multiple of [bit_mult].contents
      
  Warning : End 0 are trimed with this padding scheme *)

  (** [true] if padding scheme is a byte padding one. *)
  let byte_padding = false

  (* [pad bitset bit_mult] : Return a [bitset] with a size such as [size bitset mod bit_mult = 0] *)
  let pad bitset bit_mult =
    if _is_bitset_padded bitset bit_mult then
      bitset
    else
      let zero_bit_to_add = bit_mult - (BitSet.size bitset mod bit_mult) in
      BitSet.(concatenate bitset (zeros zero_bit_to_add))

  (** [unpad bitset] : Return the 'original' [bitset]. *)
  let unpad bitset =
    let bitset_size = BitSet.size bitset in
    let rec _loop index =
      if BitSet.bit_value index bitset then
        BitSet.extract bitset 0 (index + 1)
      else
        _loop (index - 1)
    in
    _loop (bitset_size - 1)
end

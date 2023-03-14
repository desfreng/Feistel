module type t = sig
  val byte_padding : bool
  (** [true] if padding scheme is a byte padding one. *)

  val pad : BitSet.t -> int -> BitSet.t
  (* [pad bitset mult] : Return a [bitset] with a size such as [size bitset mod mult = 0] *)

  val unpad : BitSet.t -> BitSet.t
  (* [unpad bitset] : Return the original [bitset] *)
end

module RFC_1321 : t
module ANSI_X923 : t
module PKCS7 : t
module Zero : t

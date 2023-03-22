type t

val size : t -> int
(** [size bitset] Return [bitset] size : number of [0] and [1] needed to write it. *)

val zeros : int -> t
(** [zeros size] : Get a BitSet of given [size] filled with zeros. [size] must be strictly positive. *)

val to_string : t -> string
(** Convert BitSet to a string with [0] and [1] according to its `size`
    
For the [Empty] bitset, it give an empty string. *)

val from_string : string -> t
(** [from_string str] : Return a bitset representing the bitset in [str] *)

val from_int : int -> int -> t
(** [from_int i size] :  Get a BitSet representing the integer [i] with the size [size] *)

val from_int64 : int64 -> int -> t
(** [from_int64 i size] :  Get a BitSet representing the int64 [i] with the size [size] *)

val fits_int : t -> bool
(** [fits_int bitset] : [true] if [bitset] value can be put into an integer, [false] otherwise *)

val fits_int64 : t -> bool
(** [fits_int64 bitset] : [true] if [bitset] value can be put into an int64, [false] otherwise *)

val to_int : t -> int
(** [to_int bitset] : return an integer representing [bitset] value. 
    
Raise [Invalid_argument] if [bitset] is empty or too large to be put into a integer *)

val to_int64 : t -> int64
(** [to_int64 bitset] : return an int64 representing [bitset] value. 
    
Raise [invalid_arg] if [bitset] is empty or too large to be put into an int64 *)

val from_Z : Z.t -> int -> t

val from_bytes : Bytes.t -> int -> t
(** [from_bytes bytes size] : Convert the Bytes [bytes] into a BitSet of size [size]. *)

val to_bytes : t -> Bytes.t
(** [to_bytes bitset] : Convert  [bitset] into a Bytes strcuture. *)

val bitset_not : t -> t
(** Logic not operation with [BitSet]. *)

val bitset_or : t -> t -> t
(** Logic or operation with [BitSet]. *)

val bitset_and : t -> t -> t
(** Logic and operation with [BitSet]. *)

val bitset_xor : t -> t -> t
(** Logic xor operation with [BitSet]. *)

val bitset_add : t -> t -> t
(** Modular arithmetic addition operation with [BitSet]. 
    
Non-empty BitSets must have the same [size]. *)

val bitset_sub : t -> t -> t
(** Modular arithmetic substraction operation with [BitSet]. 
    
Non-empty BitSets must have the same [size]. *)

val bitset_shift_left : t -> int -> t
(** [bitset_shift_left a i] : Shift BitSet [a] by [i] bit to the left. 
    
[a] size is unchanged by padding the right part with [0] *)

val bitset_shift_right : t -> int -> t
(** [bitset_shift_right a i] : Shift BitSet [a] by [i] bit to the right. 

[a] size is unchanged by padding the left part with [0] *)

val bitset_rotate_left : t -> int -> t
(** [bitset_rotate_left a i] : Rotate BitSet [a] by [i] bit to the left *)

val bitset_rotate_right : t -> int -> t
(** [bitset_rotate_right a i] : Rotate BitSet [a] by [i] bit to the right *)

val bit_value : int -> t -> bool
(** [bit_value index bitset] : Get the value of the [index]-nt bit of [bitset] *)

val set_bit : int -> bool -> t -> t
(** [set_bit index value bitset] : Set the value of the [index]-nt bit of [bitset] to [value] *)

val concatenate : t -> t -> t
(** Concatenate two BitSet together. The first given commes at the begining. *)

val to_bool_list : t -> bool list
(** Export all the bit of a BitSet to a [bool array] *)

val to_block : int -> t -> t list
(** [to_block block_size bitset] : Split [bitset] by chunk of [block_size] bits.
     If [bitset] doesn't have enought bits, it will be completed with [0]. First bits commes first.
     
[block_size] must be strictly positive.*)

val from_block : t list -> t
(** [from_block block_list] : return the concatenation of all the bitset in [block_list].
     First chunk commes at the begining. *)

val split : t -> int -> t * t
(** [split bitset index] : Split [bitset] in two part.
    
The first one has a size of [index] containing the Least Significant Bits of [bitset]. The second part has a size of [size bitset - index], it contains the Most Significant Bits. *)

val extract : t -> int -> int -> t
(** [extract bitset a b] : Extract and return the bitset [bitset[a:b]] *)

val pick : t -> int list -> t
(** [pick bitset index_list] : Pick bit in [bitset] and create a new BitSet according to [index_list].

The first item in the [index_list] will be the most significant bit of the new BitSet.
Exemple : [pick '0b1100' [1; 3; 0; 2] = '0b0101']*)

val byte_value : int -> t -> int
(** [byte_value byte_index bitset] : Return the value of the [byte_index]-th byte of [bitset] *)

val repeat_byte : int -> int -> t
(** [repeat_byte value number] : Return a [bitset] containing the byte [value] [number] times. 
    
The size of returned [bitset] is [8*number] *)

val equal : t -> t -> bool
(**  [equal a b] : [true] if [a] and [b] represent the same BitSet
    
Raise [Invalid_argument] if [a] and [b] are not the same size.*)

val ( = ) : t -> t -> bool
(** Alias for [equal] *)

val ( ! ) : t -> t
(** Alias for [bitset_not] *)

val ( land ) : t -> t -> t
(** Alias for [bitset_and] *)

val ( lor ) : t -> t -> t
(** Alias for [bitset_or] *)

val ( lxor ) : t -> t -> t
(** Alias for [bitset_xor] *)

val ( + ) : t -> t -> t
(** Alias for [bitset_add] *)

val ( - ) : t -> t -> t
(** Alias for [bitset_sub] *)

val ( ^ ) : t -> t -> t
(** Alias for [concatenate] *)

val ( <<< ) : t -> int -> t
(** Alias for [bitset_shift_left] *)

val ( >>> ) : t -> int -> t
(** Alias for [bitset_shift_right] *)

val ( <<> ) : t -> int -> t
(** Alias for [bitset_rotate_left] *)

val ( <>> ) : t -> int -> t
(** Alias for [bitset_rotate_right] *)

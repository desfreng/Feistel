type bitset

val size : bitset -> int
(** [size bitset] Return [bitset] size : number of [0] and [1] needed to write it. *)

val is_empty : bitset -> bool
(** [is_empty bitset] : [true] if [bitset] is empty ([size bitset = 0]), [false] otherwise. *)

val zeros : int -> bitset
(** [zeros size] : Get a BitSet of given [size] filled with zeros. [size] must be positive or null.*)

val to_string : bitset -> string
(** Convert BitSet to a string with [0] and [1] according to its `size`
    
For the [Empty] bitset, it give an empty string. *)

val from_string : string -> bitset
(** [from_string str] : Return a bitset representing the bitset in [str] *)

val from_int : int -> int -> bitset
(** [from_int i size] :  Get a BitSet representing the integer [i] with the size [size] *)

val fits_int : bitset -> bool
(** [fits_int bitset] : [true] if [bitset] value can be put into an integer, [false] otherwise *)

val to_int : bitset -> int
(** [to_int bitset] : return an integer representing [bitset] value. 
    
Raise [Invalid_argument] if [bitset] is empty or too large to be put into a integer *)

val bitset_not : bitset -> bitset
(** Logic not operation with [BitSet]. *)

val bitset_or : bitset -> bitset -> bitset
(** Logic or operation with [BitSet]. *)

val bitset_and : bitset -> bitset -> bitset
(** Logic and operation with [BitSet]. *)

val bitset_xor : bitset -> bitset -> bitset
(** Logic xor operation with [BitSet]. *)

val bitset_add : bitset -> bitset -> bitset
(** Modular arithmetic addition operation with [BitSet]. 
    
Non-empty BitSets must have the same [size]. *)

val bitset_sub : bitset -> bitset -> bitset
(** Modular arithmetic substraction operation with [BitSet]. 
    
Non-empty BitSets must have the same [size]. *)

val bitset_shift_left : bitset -> int -> bitset
(** [bitset_shift_left a i] : Shift BitSet [a] by [i] bit to the left. 
    
[a] size is unchanged by padding the right part with [0] *)

val bitset_shift_right : bitset -> int -> bitset
(** [bitset_shift_right a i] : Shift BitSet [a] by [i] bit to the right. 

[a] size is unchanged by padding the left part with [0] *)

val bitset_rotate_left : bitset -> int -> bitset
(** [bitset_rotate_left a i] : Rotate BitSet [a] by [i] bit to the left *)

val bitset_rotate_right : bitset -> int -> bitset
(** [bitset_rotate_right a i] : Rotate BitSet [a] by [i] bit to the right *)

val bit_value : int -> bitset -> bool
(** [bit_value index bitset] : Get the value of the [index]-nt bit of [bitset] *)

val set_bit : int -> bool -> bitset -> bitset
(** [set_bit index value bitset] : Set the value of the [index]-nt bit of [bitset] to [value] *)

val concatenate : bitset -> bitset -> bitset
(** Concatenate two BitSet together. The first given commes at the begining. *)

val to_bool_list : bitset -> bool list
(** Export all the bit of a BitSet to a [bool array] *)

val to_block : int -> bitset -> bitset list
(** [to_block block_size bitset] : Split [bitset] by chunk of [block_size] bits.
     If [bitset] doesn't have enought bits, it will be completed with [0]. First bits commes first.
     
[block_size] must be strictly positive.*)

val from_block : bitset list -> bitset
(** [from_block block_list] : return the concatenation of all the bitset in [block_list].
     First chunk commes at the begining. *)

val equal : bitset -> bitset -> bool
(**  [equal a b] : [true] if [a] and [b] represent the same BitSet
    
Raise [Invalid_argument] if [a] and [b] are not the same size.*)

val ( = ) : bitset -> bitset -> bool
(** Alias for [equal] *)

val ( ! ) : bitset -> bitset
(** Alias for [bitset_not] *)

val ( land ) : bitset -> bitset -> bitset
(** Alias for [bitset_and] *)

val ( lor ) : bitset -> bitset -> bitset
(** Alias for [bitset_or] *)

val ( lxor ) : bitset -> bitset -> bitset
(** Alias for [bitset_xor] *)

val ( + ) : bitset -> bitset -> bitset
(** Alias for [bitset_add] *)

val ( - ) : bitset -> bitset -> bitset
(** Alias for [bitset_sub] *)

val ( ^ ) : bitset -> bitset -> bitset
(** Alias for [concatenate] *)

val ( <<< ) : bitset -> int -> bitset
(** Alias for [bitset_shift_left] *)

val ( >>> ) : bitset -> int -> bitset
(** Alias for [bitset_shift_right] *)

val ( <<> ) : bitset -> int -> bitset
(** Alias for [bitset_rotate_left] *)

val ( <>> ) : bitset -> int -> bitset
(** Alias for [bitset_rotate_right] *)

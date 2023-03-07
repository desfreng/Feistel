type bitset = BitSet of int * Z.t | EmptyBitSet

(** [size bitset] Return [bitset] size : number of [0] and [1] needed to write it. *)
let size = function BitSet (size, _) -> size | EmptyBitSet -> 0

(** [is_empty bitset] : [true] if [bitset] is empty ([size bitset = 0]), [false] otherwise. *)
let is_empty = function BitSet _ -> false | EmptyBitSet -> true

(** Get the modulus for a given size : [2^size]

{i (bitsize is [size + 1])}*)
let _modulus size = Z.shift_left Z.one size

(** Get the max value of a BitSet for a given size : [(_modulus size) - 1]

{i (bitsize is [size])}*)
let _max_value size = Z.sub (_modulus size) Z.one

(** [zeros size] : Get a BitSet of given [size] filled with zeros. [size] must be positive or null.*)
let zeros size =
  match size with
  | 0 -> EmptyBitSet
  | n when n > 0 -> BitSet (n, Z.zero)
  | _ -> raise (Invalid_argument "Negative size provided")

(** Get a BitSet of given size filled with zeros and a one at [index]

{i (bitsize is [size])}*)
let _one_at index size =
  if index < size then
    let set = Z.shift_left Z.one index in
    BitSet (size, set)
  else
    raise (Invalid_argument "`index` must be strictly lower than `size`")

(** Get a BitSet of given size filled with ones and a zero at [index] 

{i (bitsize is [size])}*)
let _zero_at index size =
  if index < size then
    let one_everywhere = _max_value size in
    let set = Z.sub one_everywhere (Z.shift_left Z.one index) in
    BitSet (size, set)
  else
    raise (Invalid_argument "`index` must be strictly lower than `size`")

(** Convert BitSet to a string with [0] and [1] according to its `size`
    
For the [Empty] bitset, it give an empty string. *)
let to_string = function
  | BitSet (size, set) -> Z.format ("0" ^ string_of_int size ^ "b") set
  | EmptyBitSet -> ""

(** [from_string str] : Return a bitset representing the bitset in [str] *)
let from_string str =
  match String.length str with
  | 0 -> EmptyBitSet
  | size ->
      let set = Z.of_string ("0b" ^ str) in
      BitSet (size, set)

(** [from_int i size] :  Get a BitSet representing the integer [i] with the size [size] *)
let from_int i size =
  match (Z.of_int i, size) with
  | i, _ when i < Z.zero -> raise (Invalid_argument "integer must be positive")
  | i, size when i = Z.zero && size >= 1 -> BitSet (size, Z.zero)
  | i, size when size >= Z.numbits i -> BitSet (size, i)
  | _, _ -> raise (Invalid_argument "size too small")

(** [fits_int bitset] : [true] if [bitset] value can be put into an integer, [false] otherwise *)
let fits_int = function
  | BitSet (_, set) -> Z.fits_int set
  | EmptyBitSet -> false

(** [to_int bitset] : return an integer representing [bitset] value. 
    
Raise [Invalid_argument] if [bitset] is empty or too large to be put into a integer *)
let to_int = function
  | BitSet (_, set) ->
      if Z.fits_int set then
        Z.to_int set
      else
        raise (Invalid_argument "BitSet too large to be put into an integer")
  | EmptyBitSet ->
      raise (Invalid_argument "Cannot represent an EmptyBitSet in an integer")

(** General function for dealing with binary logic operation

An [EmptyBitSet] is treated as a neutral element. *)
let _bin_logic_op op left right =
  match (left, right) with
  | BitSet (size_left, set_left), BitSet (size_right, set_right) ->
      BitSet (max size_left size_right, op set_left set_right)
  | EmptyBitSet, other -> other
  | other, EmptyBitSet -> other

(** General function for dealing with binary modular arithmetic operation. 
    
An [EmptyBitSet] is treated as a neutral element. *)
let _bin_arith_op op left right =
  match (left, right) with
  | BitSet (size_left, set_left), BitSet (size_right, set_right)
    when size_left = size_right ->
      let tmp = op set_left set_right in
      let modulus = _modulus size_right in
      let rem = Z.rem (Z.add modulus (Z.rem tmp modulus)) modulus in
      BitSet (size_right, rem)
  | EmptyBitSet, other -> other
  | other, EmptyBitSet -> other
  | _, _ -> raise (Invalid_argument "Provided BitSet have different size")

(** Logic or operation with [BitSet]. *)
let bitset_or left right = _bin_logic_op Z.logor left right

(** Logic and operation with [BitSet]. *)
let bitset_and left right = _bin_logic_op Z.logand left right

(** Logic xor operation with [BitSet]. *)
let bitset_xor left right = _bin_logic_op Z.logxor left right

(** Modular arithmetic addition operation with [BitSet]. 
    
Non-empty BitSets must have the same [size]. *)
let bitset_add left right = _bin_arith_op Z.add left right

(** Modular arithmetic substraction operation with [BitSet]. 
    
Non-empty BitSets must have the same [size]. *)
let bitset_sub left right = _bin_arith_op Z.sub left right

(** Logic not operation with [BitSet]. *)
let bitset_not = function
  | BitSet (size, set) -> BitSet (size, Z.sub (_max_value size) set)
  | EmptyBitSet -> EmptyBitSet

(** [bit_value index bitset] : Get the value of the [index]-nt bit of [bitset] *)
let bit_value index bitset =
  match (index, bitset) with
  | index, BitSet (size, set) when index < size && index >= 0 ->
      Z.testbit set index
  | _ ->
      raise
        (Invalid_argument
           "`index` must be strictly lower than BitSet's size and positive")

(** [set_bit index value bitset] : Set the value of the [index]-nt bit of [bitset] to [value] *)
let set_bit index value bitset =
  match (index, bitset, value) with
  | index, BitSet (size, _), true when index < size && index > 0 ->
      bitset_or bitset (_one_at index size)
  | index, BitSet (size, _), false when index < size && index > 0 ->
      bitset_and bitset (_zero_at index size)
  | _ ->
      raise
        (Invalid_argument
           "`index` must be strictly lower than BitSet's size and positive")

(** Concatenate two BitSet together. The first given commes at the begining. *)
let concatenate begin_bitset end_bitset =
  match (begin_bitset, end_bitset) with
  | BitSet (begin_size, begin_set), BitSet (end_size, end_set) ->
      let new_set = Z.add (Z.shift_left end_set begin_size) begin_set in
      BitSet (begin_size + end_size, new_set)
  | EmptyBitSet, other -> other
  | other, EmptyBitSet -> other

(** Export all the bit of a BitSet to a [bool array] *)
let rec to_bool_list = function
  | BitSet (1, _) as bitset -> [ bit_value 0 bitset ]
  | BitSet (size, set) as bitset ->
      let next_bitset = BitSet (size - 1, Z.shift_right_trunc set 1) in
      bit_value 0 bitset :: to_bool_list next_bitset
  | EmptyBitSet -> []

(** [to_block block_size bitset] : Split [bitset] by chunk of [block_size] bits.
     If [bitset] doesn't have enought bits, it will be completed with [0]. First bits commes first.
     
[block_size] must be strictly positive.*)
let rec to_block block_size bitset =
  match (block_size, bitset) with
  | i, _ when i <= 0 ->
      raise (Invalid_argument "block_size must be strictly positive")
  | block_size, EmptyBitSet -> [ zeros block_size ]
  | block_size, BitSet (size, set) when size <= block_size ->
      [ BitSet (block_size, set) ]
  | block_size, BitSet (size, set) ->
      let next_set, new_block_set = Z.div_rem set (_modulus block_size) in
      let next_bitset = BitSet (size - block_size, next_set) in
      BitSet (block_size, new_block_set) :: to_block block_size next_bitset

(** [from_block block_list] : return the concatenation of all the bitset in [block_list].
     First chunk commes at the begining. *)
let from_block = List.fold_left concatenate EmptyBitSet

(** Alias for [bitset_and] *)
let ( &&& ) = bitset_and

(** Alias for [bitset_or] *)
let ( ||| ) = bitset_or

(** Alias for [bitset_xor] *)
let ( /// ) = bitset_xor

(** Alias for [bitset_add] *)
let ( +++ ) = bitset_add

(** Alias for [bitset_sub] *)
let ( --- ) = bitset_sub

(** Alias for [concatenate] *)
let ( ^^^ ) = concatenate

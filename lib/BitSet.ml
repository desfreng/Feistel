type t = BitSet of int * Z.t

(** [size bitset] Return [bitset] size : number of [0] and [1] needed to write it. *)
let size = function BitSet (size, _) -> size

(** Get the modulus for a given size : [2^size]

{i (bitsize is [size + 1])}*)
let _modulus size = Z.shift_left Z.one size

(** Get the max value of a BitSet for a given size : [(_modulus size) - 1]

{i (bitsize is [size])}*)
let _max_value size = Z.sub (_modulus size) Z.one

(** [zeros size] : Get a BitSet of given [size] filled with zeros. [size] must be stricly positive *)
let zeros size =
  match size with
  | n when n > 0 -> BitSet (n, Z.zero)
  | _ -> invalid_arg "Negative size provided"

(** Get a BitSet of given size filled with zeros and a one at [index]

{i (bitsize is [size])}*)
let _one_at index size =
  if index < size then
    let set = Z.shift_left Z.one index in
    BitSet (size, set)
  else
    invalid_arg "`index` must be strictly lower than `size`"

(** Get a BitSet of given size filled with ones and a zero at [index] 

{i (bitsize is [size])}*)
let _zero_at index size =
  if index < size then
    let one_everywhere = _max_value size in
    let set = Z.sub one_everywhere (Z.shift_left Z.one index) in
    BitSet (size, set)
  else
    invalid_arg "`index` must be strictly lower than `size`"

(** Convert BitSet to a string with [0] and [1] according to its `size`
    
For the [Empty] bitset, it give an empty string. *)
let to_string = function
  | BitSet (size, set) -> Z.format ("0" ^ string_of_int size ^ "b") set

(** [from_string str] : Return a bitset representing the bitset in [str] *)
let from_string str =
  match String.length str with
  | 0 -> invalid_arg "string passed must be not empty"
  | size ->
      let set = Z.of_string ("0b" ^ str) in
      BitSet (size, set)

(** [from_int i size] :  Get a BitSet representing the int [i] with the size [size] *)
let from_int i size =
  match (Z.of_int i, size) with
  | i, _ when i < Z.zero -> invalid_arg "integer must be positive"
  | i, size when i = Z.zero && size >= 1 -> BitSet (size, Z.zero)
  | i, size when size >= Z.numbits i -> BitSet (size, i)
  | _, _ -> invalid_arg "size too small"

(** [from_int64 i size] :  Get a BitSet representing the int64 [i] with the size [size] *)
let from_int64 i size =
  match (Z.of_int64 i, size) with
  | i, _ when i < Z.zero -> invalid_arg "integer must be positive"
  | i, size when i = Z.zero && size >= 1 -> BitSet (size, Z.zero)
  | i, size when size >= Z.numbits i -> BitSet (size, i)
  | _, _ -> invalid_arg "size too small"

(** [fits_int bitset] : [true] if [bitset] value can be put into an int, [false] otherwise *)
let fits_int = function BitSet (_, set) -> Z.fits_int set

(** [fits_int64 bitset] : [true] if [bitset] value can be put into an int64, [false] otherwise *)
let fits_int64 = function BitSet (_, set) -> Z.fits_int64 set

(** [to_int bitset] : return an int representing [bitset] value. 
    
Raise [invalid_arg] if [bitset] is empty or too large to be put into an int *)
let to_int = function
  | BitSet (_, set) ->
      if Z.fits_int set then
        Z.to_int set
      else
        invalid_arg "BitSet too large to be put into an integer"

(** [to_int64 bitset] : return an int64 representing [bitset] value. 
    
Raise [invalid_arg] if [bitset] is empty or too large to be put into an int64 *)
let to_int64 = function
  | BitSet (_, set) ->
      if Z.fits_int64 set then
        Z.to_int64 set
      else
        invalid_arg "BitSet too large to be put into an integer"

(** [from_Z i size] :  Get a BitSet representing the Zarith Integer [i] with the size [size] *)
let from_Z i size =
  if size >= Z.numbits i then
    BitSet (size, i)
  else
    invalid_arg "size too small"

(** [from_bytes bytes size] : Convert the Bytes [bytes] into a BitSet of size [size]. *)
let from_bytes bytes size =
  let set = Z.of_bits (Bytes.to_string bytes) in
  if size < Z.numbits set then (
    Printf.printf "%s | %i" (Z.to_string set) (Z.numbits set);
    invalid_arg "Size to small to contain provided Bytes"
  ) else
    BitSet (size, set)

(** [to_bytes bitset] : Convert  [bitset] into a Bytes strcuture. *)
let to_bytes = function BitSet (_, set) -> Z.to_bits set |> String.to_bytes

(** General function for dealing with binary logic operation

An [EmptyBitSet] is treated as a neutral element. *)
let _bin_logic_op op left right =
  match (left, right) with
  | BitSet (size_left, set_left), BitSet (size_right, set_right)
    when size_left = size_right ->
      BitSet (size_left, op set_left set_right)
  | _, _ -> invalid_arg "Provided BitSet have different size"

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
  | _, _ -> invalid_arg "Provided BitSet have different size"

(** Logic not operation with [BitSet]. *)
let bitset_not = function
  | BitSet (size, set) -> BitSet (size, Z.sub (_max_value size) set)

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

(** [bitset_shift_left a i] : Shift BitSet [a] by [i] bit to the left. 
    
[a] size is unchanged by padding the right part with [0] *)
let bitset_shift_left bitset shift =
  match (bitset, shift) with
  | _, 0 -> bitset
  | _, i when i < 0 -> invalid_arg "Shift amount must be positive"
  | BitSet (size, set), shift ->
      let shift_amount = shift mod size in
      let modulus = _modulus size in
      let new_set = Z.rem (Z.shift_left set shift_amount) modulus in
      BitSet (size, new_set)

(** [bitset_shift_right a i] : Shift BitSet [a] by [i] bit to the right. 

[a] size is unchanged by padding the left part with [0] *)
let bitset_shift_right bitset shift =
  match (bitset, shift) with
  | _, 0 -> bitset
  | _, i when i < 0 -> invalid_arg "Shift amount must be positive"
  | BitSet (size, set), shift ->
      let shift_amount = shift mod size in
      let modulus = _modulus size in
      let new_set = Z.rem (Z.shift_right_trunc set shift_amount) modulus in
      BitSet (size, new_set)

(** [bitset_rotate_left a i] : Rotate BitSet [a] by [i] bit to the left *)
let bitset_rotate_left bitset shift =
  match (bitset, shift) with
  | _, 0 -> bitset
  | _, i when i < 0 -> invalid_arg "Shift amount must be positive"
  | bitset, shift ->
      let bitset_size = size bitset in
      let shift_amount = shift mod bitset_size in
      let left_part = bitset_shift_left bitset shift_amount in
      let right_part = bitset_shift_right bitset (bitset_size - shift_amount) in
      bitset_or left_part right_part

(** [bitset_rotate_right a i] : Rotate BitSet [a] by [i] bit to the right *)
let bitset_rotate_right bitset shift =
  match (bitset, shift) with
  | _, 0 -> bitset
  | _, i when i < 0 -> invalid_arg "Shift amount must be positive"
  | bitset, shift ->
      let bitset_size = size bitset in
      let shift_amount = shift mod bitset_size in
      let right_part = bitset_shift_right bitset shift_amount in
      let left_part = bitset_shift_left bitset (bitset_size - shift_amount) in
      bitset_or left_part right_part

(** [bit_value index bitset] : Get the value of the [index]-nt bit of [bitset] *)
let bit_value index bitset =
  match (index, bitset) with
  | index, BitSet (size, set) when index < size && index >= 0 ->
      Z.testbit set index
  | _ ->
      invalid_arg
        "`index` must be strictly lower than BitSet's size and positive"

(** [set_bit index value bitset] : Set the value of the [index]-nt bit of [bitset] to [value] *)
let set_bit index value bitset =
  match (index, bitset, value) with
  | index, BitSet (size, _), true when index < size && index >= 0 ->
      bitset_or bitset (_one_at index size)
  | index, BitSet (size, _), false when index < size && index >= 0 ->
      bitset_and bitset (_zero_at index size)
  | _ ->
      invalid_arg
        "`index` must be strictly lower than BitSet's size and positive"

(** Concatenate two BitSet together. The first given commes at the begining. *)
let concatenate begin_bitset end_bitset =
  match (begin_bitset, end_bitset) with
  | BitSet (begin_size, begin_set), BitSet (end_size, end_set) ->
      let new_set = Z.add (Z.shift_left end_set begin_size) begin_set in
      BitSet (begin_size + end_size, new_set)

(** Export all the bit of a BitSet to a [bool array] *)
let rec to_bool_list = function
  | BitSet (1, _) as bitset -> [ bit_value 0 bitset ]
  | BitSet (size, set) as bitset ->
      let next_bitset = BitSet (size - 1, Z.shift_right_trunc set 1) in
      bit_value 0 bitset :: to_bool_list next_bitset

(** [to_block block_size bitset] : Split [bitset] by chunk of [block_size] bits.
     If [bitset] doesn't have enought bits, it will be completed with [0]. First bits commes first.
     
[block_size] must be strictly positive.*)
let to_block block_size bitset =
  if block_size < 0 then
    invalid_arg "block_size must be strictly positive"
  else if size bitset mod block_size != 0 then
    invalid_arg "bitset size must be a multiple of block_size"
  else
    let rec _loop remaining_bitset =
      match remaining_bitset with
      | BitSet (size, _) when size = block_size -> [ remaining_bitset ]
      | BitSet (size, set) when size > block_size ->
          let next_set, new_block_set = Z.div_rem set (_modulus block_size) in
          let next_bitset = BitSet (size - block_size, next_set) in
          BitSet (block_size, new_block_set) :: _loop next_bitset
      | _ -> failwith "Error ?!"
    in
    _loop bitset

(** [from_block block_list] : return the concatenation of all the bitset in [block_list].
     First chunk commes at the begining. 

[block_list] must be a non-empty list. *)
let from_block = function
  | [] -> invalid_arg "provided list must not be empty"
  | hd :: tl -> List.fold_left concatenate hd tl

(** [split bitset index] : Split [bitset] in two part.
    
The first one has a size of [index] containing the Least Significant Bits of [bitset]. The second part has a size of [size bitset - index], it contains the Most Significant Bits. *)
let split bitset index =
  if index <= 0 || index >= size bitset then
    invalid_arg
      "`index` must be between 0 excluded and the bitset's size excluded."
  else
    match bitset with
    | BitSet (size, set) ->
        let lsb_modulus = _modulus index in
        let msb_set, lsb_set = Z.div_rem set lsb_modulus in
        (BitSet (index, lsb_set), BitSet (size - index, msb_set))

(** [extract bitset a b] : Extract and return the bitset [bitset[a:b]] *)
let extract bitset a b =
  if a >= b then
    invalid_arg "`a` must be strictly lesser than `b` for extraction."
  else if a < 0 || b > size bitset then
    invalid_arg "`[a:b]` must be contained in `[0:size bitset]."
  else
    match bitset_shift_right bitset a with
    | BitSet (_, set) ->
        let new_size = b - a in
        let new_modulus = _modulus new_size in
        let new_set = Z.rem set new_modulus in
        BitSet (new_size, new_set)

(** [byte_value byte_index bitset] : Return the value of the [byte_index]-th byte of [bitset] *)
let byte_value byte_index bitset =
  if byte_index < 0 then
    invalid_arg "Byte index must be positive or null"
  else if (8 * byte_index) + 8 > size bitset then
    invalid_arg ("`bitset` has no bytes with index " ^ string_of_int byte_index)
  else
    to_int (extract bitset (8 * byte_index) ((8 * byte_index) + 8))

(** [repeat_byte value number] : Return a [bitset] containing the byte [value] [number] times. 
    
The size of returned [bitset] is [8*number] *)
let rec repeat_byte value number =
  if value < 0 || value > 0xff then
    invalid_arg "Byte value must be in [0; 255]"
  else if number <= 0 then
    invalid_arg "Byte repeatition times must be strictly positive"
  else if number = 1 then
    from_int value 8
  else
    concatenate (from_int value 8) (repeat_byte value (number - 1))

(** [pick bitset index_list] : Pick bit in [bitset] and create a new BitSet according to [index_list].

The first item in the [index_list] will be the most significant bit of the new BitSet.
Exemple : [pick '0b1100' [1; 3; 0; 2] = '0b0101']*)
let pick bitset index_list =
  let new_bitset_size = List.length index_list in
  fst
    (List.fold_left
       (fun (acc, index) elm ->
         (set_bit index (bit_value elm bitset) acc, index + 1))
       (zeros new_bitset_size, 0)
       index_list)

(**  [equal a b] : [true] if [a] and [b] represent the same BitSet
    
Raise [invalid_arg] if [a] and [b] are not the same size.*)
let equal bitset1 bitset2 =
  match (bitset1, bitset2) with
  | BitSet (size1, set1), BitSet (size2, set2) when size1 = size2 ->
      Z.equal set1 set2
  | BitSet (size1, _), BitSet (size2, _) when size1 != size2 ->
      invalid_arg "BitSet must have the same size"
  | _ -> false

(** Alias for [equal] *)
let ( = ) a b = equal a b

(** Alias for [bitset_not] *)
let ( ! ) bitset = bitset_not bitset

(** Alias for [bitset_and] *)
let ( land ) a b = bitset_and a b

(** Alias for [bitset_or] *)
let ( lor ) a b = bitset_or a b

(** Alias for [bitset_xor] *)
let ( lxor ) a b = bitset_xor a b

(** Alias for [bitset_add] *)
let ( + ) a b = bitset_add a b

(** Alias for [bitset_sub] *)
let ( - ) a b = bitset_sub a b

(** Alias for [concatenate] *)
let ( ^ ) a b = concatenate b a

(** Alias for [bitset_shift_left] *)
let ( <<< ) a b = bitset_shift_left a b

(** Alias for [bitset_shift_right] *)
let ( >>> ) a b = bitset_shift_right a b

(** Alias for [bitset_rotate_left] *)
let ( <<> ) a b = bitset_rotate_left a b

(** Alias for [bitset_rotate_right] *)
let ( <>> ) a b = bitset_rotate_right a b

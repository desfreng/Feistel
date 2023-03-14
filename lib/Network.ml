module type t = sig
  type key
  type round_key

  val begin_enc_function : BitSet.t -> BitSet.t
  val end_enc_function : BitSet.t -> BitSet.t
  val begin_dec_function : BitSet.t -> BitSet.t
  val end_dec_function : BitSet.t -> BitSet.t
  val number_of_round : int
  val to_key : BitSet.t -> key
  val gen_round_key : key -> int -> round_key
  val lsb_part_size : int
  val msb_part_size : int
  val round_function : round_key -> BitSet.t -> BitSet.t
end

module Make (Net : t) = struct
  type key = Net.key

  (** Return the key encoded by provided bitset *)
  let to_key = Net.to_key

  (** Data block size *)
  let _block_size = Net.lsb_part_size + Net.msb_part_size

  (** [_split bitset] : split [bitset] in two part.
      
  The first one is the least significant bits part.
  The secound is the most significant bits part. *)
  let _enc_split bitset = BitSet.split bitset Net.lsb_part_size

  let _dec_split bitset = BitSet.split bitset Net.msb_part_size

  (** [_permute bitset boolean] : if [boolean] then split [bitset], swap and 
      concatenate the two part. Do nothing otherwise. *)
  let _permute bitset boolean =
    if boolean then
      let lsb, msb = BitSet.split bitset (_block_size / 2) in
      BitSet.concatenate msb lsb
    else
      bitset

  let _check_data plaintext =
    if BitSet.size plaintext != _block_size then
      invalid_arg ("`plaintext` must be of size " ^ string_of_int _block_size)

  (** [decrypt master_key bitset] : Decrypt [bitset] by appliying rounds of the Feistel Network with the key [master_key] on the [bitset]. *)
  let encrypt master_key plaintext =
    let rec _loop bitset round_id =
      match round_id with
      | i when i < 0 -> invalid_arg "`round_id` must be positive"
      | i when i >= Net.number_of_round -> bitset
      | i ->
          let round_key = Net.gen_round_key master_key i in
          let lsb_i, msb_i = _enc_split bitset in
          let lsb_ii, msb_ii =
            (BitSet.(msb_i lxor Net.round_function round_key lsb_i), lsb_i)
          in
          let round_bitset = BitSet.concatenate lsb_ii msb_ii in
          _loop round_bitset (round_id + 1)
    in
    _check_data plaintext;
    let _pre_encr = Net.begin_enc_function plaintext in
    let _after_encr = _loop _pre_encr 0 in
    Net.end_enc_function _after_encr

  (** [decrypt master_key bitset] : Decrypt [bitset] by reverse appliying rounds of the Feistel Network with the key [master_key] on the [bitset]. *)
  let decrypt master_key ciphertext =
    let rec _loop bitset round_id =
      match round_id with
      | i when i >= Net.number_of_round ->
          invalid_arg "`round_id` must be smaller than the number of round"
      | i when i <= -1 -> bitset
      | i ->
          let round_key = Net.gen_round_key master_key i in
          let lsb_ii, msb_ii = _dec_split bitset in
          let lsb_i, msb_i =
            (msb_ii, BitSet.(lsb_ii lxor Net.round_function round_key msb_ii))
          in
          let round_bitset = BitSet.concatenate lsb_i msb_i in
          _loop round_bitset (round_id - 1)
    in
    _check_data ciphertext;
    let _pre_decr = Net.begin_dec_function ciphertext in
    let _after_decr = _loop _pre_decr (Net.number_of_round - 1) in
    Net.end_dec_function _after_decr
end

module Lucifer : t = struct
  type key = Key of BitSet.t
  type round_key = RoundKey of BitSet.t * BitSet.t

  let begin_enc_function bitset =
    let lsb, msb = BitSet.split bitset 64 in
    BitSet.concatenate msb lsb

  let end_enc_function a = a
  let begin_dec_function = end_enc_function
  let end_dec_function = begin_enc_function
  let number_of_round = 16

  let to_key bitset =
    if BitSet.size bitset != 128 then
      invalid_arg "Key must be 128 bits long"
    else
      Key bitset

  let _sub0 input =
    match input with
    | 0 -> 12
    | 1 -> 15
    | 2 -> 7
    | 3 -> 10
    | 4 -> 14
    | 5 -> 13
    | 6 -> 11
    | 7 -> 0
    | 8 -> 2
    | 9 -> 6
    | 10 -> 3
    | 11 -> 1
    | 12 -> 9
    | 13 -> 4
    | 14 -> 5
    | 15 -> 8
    | _ -> invalid_arg "integer must be in [0; 15]"

  let _sub1 input =
    match input with
    | 0 -> 7
    | 1 -> 2
    | 2 -> 14
    | 3 -> 9
    | 4 -> 3
    | 5 -> 11
    | 6 -> 0
    | 7 -> 4
    | 8 -> 12
    | 9 -> 13
    | 10 -> 1
    | 11 -> 10
    | 12 -> 6
    | 13 -> 15
    | 14 -> 8
    | 15 -> 5
    | _ -> invalid_arg "integer must be in [0; 15]"

  let _perm input =
    if BitSet.size input != 64 then
      failwith "Error"
    else
      BitSet.pick input
        [
          53;
          42;
          11;
          7;
          36;
          62;
          16;
          25;
          45;
          34;
          3;
          63;
          28;
          54;
          8;
          17;
          37;
          26;
          59;
          55;
          20;
          46;
          0;
          9;
          29;
          18;
          51;
          47;
          12;
          38;
          56;
          1;
          21;
          10;
          43;
          39;
          4;
          30;
          48;
          57;
          13;
          2;
          35;
          31;
          60;
          22;
          40;
          49;
          5;
          58;
          27;
          23;
          52;
          14;
          32;
          41;
          61;
          50;
          19;
          15;
          44;
          6;
          24;
          33;
        ]

  let gen_round_key key round =
    match key with
    | Key k ->
        let rotated_key = BitSet.bitset_rotate_left k (7 * round) in
        RoundKey
          (BitSet.extract rotated_key 0 8, BitSet.extract rotated_key 0 64)

  let lsb_part_size = 64
  let msb_part_size = 64

  let round_function round_key bitset =
    match round_key with
    | RoundKey (icb, xor_k) ->
        let block8_list = BitSet.(to_block 8 (xor_k lxor bitset)) in
        let s_bytes_list =
          snd
            (List.fold_left
               (fun (index, acc) elm ->
                 let nibbles = BitSet.split elm 4 in
                 let msb_nibble, lsb_nibble =
                   if BitSet.bit_value index icb then
                     (BitSet.to_int (snd nibbles), BitSet.to_int (fst nibbles))
                   else
                     (BitSet.to_int (fst nibbles), BitSet.to_int (snd nibbles))
                 in
                 let new_block8 =
                   BitSet.(
                     concatenate
                       (from_int (_sub1 lsb_nibble) 4)
                       (from_int (_sub0 msb_nibble) 4))
                 in
                 (index + 1, new_block8 :: acc))
               (0, []) block8_list)
        in
        _perm (BitSet.from_block s_bytes_list)
end

module SDES : t = struct
  type key = Key of BitSet.t
  type round_key = RoundKey of BitSet.t

  let begin_enc_function bitset = BitSet.pick bitset [ 6; 2; 5; 7; 4; 0; 3; 1 ]
  let end_enc_function bitset = BitSet.(bitset <<> 1)
  let begin_dec_function bitset = BitSet.(bitset <>> 1)
  let end_dec_function bitset = BitSet.pick bitset [ 4; 7; 5; 3; 1; 6; 0; 2 ]
  let number_of_round = 2
  let _p10 bitset = BitSet.pick bitset [ 7; 5; 8; 3; 6; 0; 9; 1; 2; 4 ]
  let _p8 bitset = BitSet.pick bitset [ 4; 7; 3; 6; 2; 5; 0; 1 ]
  let _p4 bitset = BitSet.pick bitset [ 2; 0; 1; 3 ]

  let _rot_i bitset amount =
    let lsb, msb = BitSet.split bitset 5 in
    BitSet.(concatenate (lsb <<> amount) (msb <<> amount))

  let _s0 bitset =
    let tmp =
      match BitSet.to_int bitset with
      | 2 | 7 | 8 -> 0
      | 0 | 5 | 11 | 12 -> 1
      | 3 | 6 | 10 | 15 -> 2
      | 1 | 4 | 9 | 13 | 14 -> 3
      | _ -> invalid_arg "integer must be between 0 and 15"
    in
    BitSet.from_int tmp 2

  let _s1 bitset =
    let tmp =
      match BitSet.to_int bitset with
      | 0 | 3 | 10 | 13 | 14 -> 0
      | 2 | 5 | 11 | 12 -> 1
      | 1 | 4 | 9 -> 2
      | 6 | 7 | 8 | 15 -> 3
      | _ -> invalid_arg "integer must be between 0 and 1555555d"
    in
    BitSet.from_int tmp 2

  let to_key bitset =
    if BitSet.size bitset != 10 then
      invalid_arg "BitSet size must be 10 to be considered as a SDES key."
    else
      Key bitset

  let gen_round_key master_key round =
    match (master_key, round) with
    | Key key_bitset, 0 -> RoundKey (_p8 (_rot_i (_p10 key_bitset) 1))
    | Key key_bitset, 1 -> RoundKey (_p8 (_rot_i (_p10 key_bitset) 3))
    | _ ->
        invalid_arg
          ("Round can only be between 0 and "
          ^ string_of_int (number_of_round - 1))

  let lsb_part_size = 4
  let msb_part_size = 4

  let round_function round_key bitset =
    match round_key with
    | RoundKey round_key_bitset ->
        let expanded_bitset = BitSet.pick bitset [ 0; 3; 2; 1; 2; 1; 0; 3 ] in
        let lsb, msb =
          BitSet.(split (expanded_bitset lxor round_key_bitset) 4)
        in
        _p4 (BitSet.concatenate (_s1 lsb) (_s0 msb))
end

module SimpleSP96 : t = struct
  type key = Key of BitSet.t
  type round_key = RoundKey of BitSet.t

  let begin_enc_function bitset = bitset
  let end_enc_function bitset = bitset
  let begin_dec_function bitset = bitset
  let end_dec_function bitset = bitset
  let number_of_round = 16

  let _s index bitset =
    let tmp =
      match (BitSet.to_int bitset + index) mod 16 with
      | 0 -> 6
      | 1 -> 5
      | 2 -> 11
      | 3 -> 10
      | 4 -> 2
      | 5 -> 4
      | 6 -> 15
      | 7 -> 9
      | 8 -> 7
      | 9 -> 8
      | 10 -> 13
      | 11 -> 12
      | 12 -> 1
      | 13 -> 3
      | 14 -> 14
      | 15 -> 0
      | _ -> invalid_arg "integer must be in [0; 15]"
    in
    BitSet.from_int tmp 4

  let _p bitset =
    BitSet.pick bitset
      [
        8;
        18;
        3;
        62;
        16;
        55;
        35;
        32;
        29;
        40;
        43;
        23;
        45;
        57;
        28;
        22;
        30;
        19;
        31;
        37;
        41;
        60;
        14;
        20;
        52;
        21;
        1;
        58;
        42;
        27;
        17;
        9;
        12;
        50;
        56;
        0;
        7;
        13;
        61;
        53;
        24;
        5;
        39;
        2;
        46;
        10;
        36;
        54;
        47;
        48;
        34;
        26;
        49;
        63;
        4;
        15;
        6;
        51;
        38;
        44;
        59;
        25;
        11;
        33;
      ]

  let to_key bitset =
    if BitSet.size bitset != 96 then
      invalid_arg "BitSet size must be 96 to be considered as a SimpleSP96 key."
    else
      Key bitset

  let gen_round_key master_key round =
    match (master_key, round) with
    | _, i when i < 0 || i >= number_of_round ->
        invalid_arg
          ("Round can only be between 0 and "
          ^ string_of_int (number_of_round - 1))
    | Key key_bitset, i ->
        let lsb, _ = BitSet.(split (key_bitset <<> 7 * i) 64) in
        RoundKey lsb

  let lsb_part_size = 64
  let msb_part_size = 32

  let round_function round_key bitset =
    match round_key with
    | RoundKey round_key_bitset ->
        let chunk_bitset = BitSet.(to_block 4 (bitset lxor round_key_bitset)) in
        let s_bitset =
          fst
            (List.fold_left
               (fun (acc, index) elm -> (_s index elm :: acc, index + 1))
               ([], 0) chunk_bitset)
        in
        let p_bitset = _p (BitSet.from_block s_bitset) in
        let lsb, msb = BitSet.(split p_bitset 32) in
        BitSet.(lsb lxor msb)
end

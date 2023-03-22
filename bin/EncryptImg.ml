open Feistel
module Json = Yojson.Basic

module type T = functor (C : Cipher.t) -> OperationMode.t with type key = C.key

let check_file filename =
  if not (Sys.file_exists filename) then
    failwith ("The file : '" ^ filename ^ "' does not exists !")

let extract_args () =
  let args = Sys.argv in
  if Array.length args < 3 then
    failwith "2 Arguments Expected"
  else
    let in_f = args.(1) in
    let out_f = args.(2) in
    check_file in_f;
    (in_f, out_f)

let encode (module S : CipherSuite.t) iv data key =
  let cipher_key = S.build_key key in
  S.encrypt iv data cipher_key

let decode (module S : CipherSuite.t) iv data key =
  let cipher_key = S.build_key key in
  let res = S.decrypt iv data cipher_key in
  res

let process_padding padding =
  match padding with
  | "Zero" -> (module Padding.Zero : Padding.t)
  | "ANSI_X923" -> (module Padding.ANSI_X923)
  | "PKCS7" -> (module Padding.PKCS7)
  | "RFC_1321" -> (module Padding.RFC_1321)
  | _ -> failwith "Wrong Padding"

let process_mode mode =
  match mode with
  | "ECB" -> (module OperationMode.ECB : T)
  | "CBC" -> (module OperationMode.CBC)
  | "CTR" -> (module OperationMode.CTR)
  | _ -> failwith "Wrong Mode"

let process_cipher cipher =
  match cipher with
  | "Lucifer" -> (module Cipher.Lucifer : Cipher.t)
  | "SDES" -> (module Cipher.SDES)
  | "SimpleSP96" -> (module Cipher.SimpleSP96)
  | _ -> failwith "Wrong Cipher"

let parse_bitset_from_json json_data =
  let open Json.Util in
  let bitset_size = json_data |> member "size" |> to_int in
  let byte_data = json_data |> member "bytes" |> to_list |> List.map to_int in
  let raw_size = 32 * List.length byte_data in
  let raw_bitset =
    List.fold_left
      (fun (index, acc) elm ->
        let bitset_value = BitSet.from_int elm raw_size in
        let shifted_value = BitSet.(bitset_value <<< index * 32) in
        (index + 1, BitSet.(shifted_value lor acc)))
      (0, BitSet.zeros raw_size)
      byte_data
    |> snd
  in
  if raw_size < bitset_size then
    BitSet.concatenate raw_bitset (BitSet.zeros (bitset_size - raw_size))
  else
    BitSet.extract raw_bitset 0 bitset_size

let export_bitset_to_json bitset =
  let size = BitSet.size bitset in
  let rec _loop bitset =
    let bitset_size = BitSet.size bitset in
    if bitset_size <= 32 then
      [ BitSet.to_int bitset ]
    else
      let new_value = BitSet.to_int (BitSet.extract bitset 0 32) in
      let next_bitset = BitSet.extract bitset 32 bitset_size in
      new_value :: _loop next_bitset
  in
  let int_json_list = List.map (fun elm -> `Int elm) (_loop bitset) in
  `Assoc [ ("size", `Int size); ("bytes", `List int_json_list) ]

let parse_json json_file =
  let json_data = Json.from_file json_file in
  let open Json.Util in
  let data = parse_bitset_from_json (member "data" json_data) in
  let key = parse_bitset_from_json (member "key" json_data) in
  let iv = parse_bitset_from_json (member "iv" json_data) in
  let cipher = json_data |> member "cipher" |> to_string |> process_cipher in
  let mode = json_data |> member "mode" |> to_string |> process_mode in
  let padding = json_data |> member "padding" |> to_string |> process_padding in
  let do_encryption = json_data |> member "do_encryption" |> to_bool in
  (iv, data, key, cipher, mode, padding, do_encryption)

let export_res out_file data =
  let json_data = export_bitset_to_json data in
  let oc = open_out out_file in
  Json.to_channel oc json_data;
  output_string oc "\n";
  close_out oc

let main () =
  Printf.printf "\nStarting... ";
  flush stdout;
  let input_file, output_file = extract_args () in
  Printf.printf "Done\nParsing JSON... ";
  flush stdout;
  let iv, data, key, cipher, mode, padding, do_encryption =
    parse_json input_file
  in
  Printf.printf "Done\nSetting up Modules... ";
  flush stdout;
  let module Suite =
    CipherSuite.MakeSuite ((val cipher)) ((val mode)) ((val padding))
  in
  let out_data =
    if do_encryption then (
      Printf.printf "Done\nEncryption... ";
      flush stdout;
      encode (module Suite) iv data key
    ) else (
      Printf.printf "Done\nDecryption... ";
      flush stdout;
      decode (module Suite) iv data key
    )
  in
  Printf.printf "Done\nExporting Data... ";
  flush stdout;
  export_res output_file out_data;
  Printf.printf "Done\n\nThe End.\n\n";
  flush stdout

let () =
  Printexc.record_backtrace true;
  main ()

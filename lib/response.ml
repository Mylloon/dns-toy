open Types
open Utils

let parse_header (reader : reader) =
  let max_size = 6 in
  match List.init max_size (fun offset -> unpack_short_be reader.data (offset * 2)) with
  | [ id; flags; num_questions; num_answers; num_authorities; num_additionals ] ->
    ( { reader with pointer = reader.pointer + (max_size * 2) }
    , { id; flags; num_questions; num_answers; num_authorities; num_additionals } )
  | _ -> failwith "Invalid number of fields"
;;

let rec parse_question (reader : reader) =
  let name, offset_name = decode_name reader in
  let data = bytes_forward reader.data (reader.pointer + offset_name) in
  let max_size = 2 in
  match List.init max_size (fun offset -> unpack_short_be data (offset * 2)) with
  | [ type_; class_ ] ->
    ( { reader with pointer = reader.pointer + offset_name + (max_size * 2) }
    , { name; type_; class_ } )
  | _ -> failwith "Invalid number of fields"

and decode_name reader =
  let rec read_parts parts pos =
    let length = int_of_char (Bytes.get reader.data (reader.pointer + pos)) in
    if length = 0
    then 1, parts
    else if length land 0b1100_0000 <> 0
    then (
      let decoded_name = decode_compressed_name reader length in
      2, decoded_name :: parts)
    else (
      let part = Bytes.sub_string reader.data (reader.pointer + pos + 1) length in
      let last_length, parts' = read_parts (parts @ [ part ]) (pos + 1 + length) in
      last_length + length + 1, parts')
  in
  let offset, parts = read_parts [] 0 in
  String.to_bytes (String.concat "." parts), offset

and decode_compressed_name reader length =
  let pointer =
    (length land 0b0011_1111) + int_of_char (Bytes.get reader.data (reader.pointer + 1))
  in
  String.of_bytes (fst (decode_name { reader with pointer }))
;;

let parse_record reader =
  let name, offset_name = decode_name reader in
  let data = bytes_forward reader.data (offset_name + reader.pointer) in
  let data_len = unpack_short_be data 8 in
  let record_len = 10 in
  ( { reader with pointer = reader.pointer + offset_name + record_len + data_len }
  , { name
    ; type_ = unpack_short_be data 0
    ; class_ = unpack_short_be data 2
    ; ttl = unpack_int_be data 4
    ; data = Bytes.sub data record_len data_len
    } )
;;

let parse_dns_packet data =
  let reader, dns_header = parse_header { data; pointer = 0 } in
  (* Quid du pointeur du reader, pas récupéré à chaque itération... *)
  { header = dns_header
  ; questions = List.init dns_header.num_questions (fun _ -> snd (parse_question reader))
  ; answers = List.init dns_header.num_answers (fun _ -> snd (parse_record reader))
  ; authorities =
      List.init dns_header.num_authorities (fun _ -> snd (parse_record reader))
  ; additionals =
      List.init dns_header.num_additionals (fun _ -> snd (parse_record reader))
  }
;;

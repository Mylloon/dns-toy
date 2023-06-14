open Types
open Utils

let parse_header reader =
  let max_size = 6 in
  match List.init max_size (fun offset -> unpack_short_be reader.data (offset * 2)) with
  | [ id; flags; num_questions; num_answers; num_authorities; num_additionals ] ->
    ( { reader with pointer = reader.pointer + (max_size * 2) }
    , { id; flags; num_questions; num_answers; num_authorities; num_additionals } )
  | _ -> failwith "Invalid number of fields"
;;

let rec parse_question reader =
  let name, offset_name = decode_name_simple reader in
  let data = bytes_forward reader.data (reader.pointer + offset_name) in
  let max_size = 2 in
  match List.init max_size (fun offset -> unpack_short_be data (offset * 2)) with
  | [ type_; class_ ] ->
    ( { reader with pointer = reader.pointer + offset_name + (max_size * 2) }
    , { name; type_; class_ } )
  | _ -> failwith "Invalid number of fields"

and decode_name_simple reader =
  let rec read_parts parts pos =
    let length = int_of_char (Bytes.get reader.data (reader.pointer + pos)) in
    if length = 0
    then 1, parts
    else (
      let part = Bytes.sub_string reader.data (reader.pointer + pos + 1) length in
      let last_length, parts' = read_parts (parts @ [ part ]) (pos + 1 + length) in
      last_length + length + 1, parts')
  in
  let offset, parts = read_parts [] 0 in
  String.to_bytes (String.concat "." parts), offset
;;

let parse_record _reader =
  let name, _offset_name = Bytes.empty, 0 (* decode_name_simple reader *) in
  let type_, class_, ttl, data = 0, 0, 0, Bytes.empty in
  { name; type_; class_; ttl; data }
;;

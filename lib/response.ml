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
    else if length land 0b1100_0000 <> 0
    then (
      let decoded_name = decode_compressed_name reader length in
      length + 1, decoded_name :: parts)
    else (
      let part = Bytes.sub_string reader.data (reader.pointer + pos + 1) length in
      let last_length, parts' = read_parts (parts @ [ part ]) (pos + 1 + length) in
      last_length + length + 1, parts')
  in
  let offset, parts = read_parts [] 0 in
  String.to_bytes (String.concat "." parts), offset

(*
---
def decode_compressed_name(length, reader):
    pointer_bytes = bytes([length & 0b0011_1111]) + reader.read(1)
    pointer = struct.unpack("!H", pointer_bytes)[0]
    current_pos = reader.tell()
    reader.seek(pointer)
    result = decode_name(reader)
    reader.seek(current_pos)
    return result
---
*)
and decode_compressed_name _reader _length =
  (* TODO *)
  String.empty
;;

(*
---
def parse_record(reader):
    name = decode_name_simple(reader)
    # the the type, class, TTL, and data length together are 10 bytes (2 + 2 + 4 + 2 = 10)
    # so we read 10 bytes
    data = reader.read(10)
    # HHIH means 2-byte int, 2-byte-int, 4-byte int, 2-byte int
    type_, class_, ttl, data_len = struct.unpack("!HHIH", data)
    data = reader.read(data_len)
    return DNSRecord(name, type_, class_, ttl, data)
---
*)
let parse_record reader =
  let name, _offset_name = decode_name_simple reader in
  (* TODO *)
  let type_, class_, ttl, data = 0, 0, 0, Bytes.empty in
  { name; type_; class_; ttl; data }
;;

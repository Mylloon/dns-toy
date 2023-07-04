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
      let decoded_name =
        decode_compressed_name { reader with pointer = reader.pointer + pos } length
      in
      2, decoded_name :: parts)
    else (
      let part = Bytes.sub_string reader.data (reader.pointer + pos + 1) length in
      let last_length, parts' = read_parts (parts @ [ part ]) (pos + 1 + length) in
      last_length + length + 1, parts')
  in
  let offset, parts = read_parts [] 0 in
  String.to_bytes (String.concat "." parts), offset

and decode_compressed_name reader length =
  let pointer_bytes =
    Bytes.cat
      (int_to_bytes (length land 0b0011_1111))
      (Bytes.sub reader.data (reader.pointer + 1) 1)
  in
  let pointer = unpack_short_be pointer_bytes 0 in
  String.of_bytes (fst (decode_name { reader with pointer }))
;;

let parse_record reader =
  let name, offset_name = decode_name reader in
  let data = bytes_forward reader.data (offset_name + reader.pointer) in
  let data_len = unpack_short_be data 8 in
  let record_len = 10 in
  let type_ = unpack_short_be data 0 in
  ( { reader with pointer = reader.pointer + offset_name + record_len + data_len }
  , { name
    ; type_
    ; class_ = unpack_short_be data 2
    ; ttl = unpack_int_be data 4
    ; data =
        (let raw_data = Bytes.sub data record_len data_len in
         match type_ with
         | t when t = DNSType.ns ->
           fst
             (decode_name
                { reader with pointer = reader.pointer + offset_name + record_len })
         | t when t = DNSType.a -> String.to_bytes (get_ip raw_data)
         | _ -> raw_data)
    } )
;;

let parse_dns_packet data =
  let rec create_list reader n fn acc =
    if n = 0
    then reader, List.rev acc
    else (
      let next_reader, res = fn reader in
      create_list next_reader (n - 1) fn (res :: acc))
  in
  let reader, header = parse_header { data; pointer = 0 } in
  let reader2, questions = create_list reader header.num_questions parse_question [] in
  let reader3, answers = create_list reader2 header.num_answers parse_record [] in
  let reader4, authorities = create_list reader3 header.num_authorities parse_record [] in
  let _, additionals = create_list reader4 header.num_additionals parse_record [] in
  (* print_endline (Debug.dns_header header);
  List.iter (fun el -> print_endline (Debug.dns_question el)) questions;
  List.iter (fun el -> print_endline (Debug.dns_record el)) answers;
  List.iter (fun el -> print_endline (Debug.dns_record el)) authorities;
  List.iter (fun el -> print_endline (Debug.dns_record el)) additionals; *)
  { header; questions; answers; authorities; additionals }
;;

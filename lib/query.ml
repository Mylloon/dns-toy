open Types
open Utils

let header_to_bytes header =
  new_buffer
    header
    [ header.id
    ; header.flags
    ; header.num_questions
    ; header.num_answers
    ; header.num_authorities
    ; header.num_additionals
    ]
;;

let question_to_bytes question =
  let buffer1 = new_buffer question [ question.type_; question.class_ ] in
  let buffer2 = Buffer.create (Bytes.length question.name + Bytes.length buffer1) in
  Buffer.add_bytes buffer2 question.name;
  Buffer.add_bytes buffer2 buffer1;
  buffer2
;;

let encode_dns_name domain_name =
  let parts = String.split_on_char '.' domain_name in
  let encoded_parts =
    List.map
      (fun part ->
        let len_part = String.length part in
        let len_byte = Char.chr len_part in
        Bytes.cat (Bytes.of_string (String.make 1 len_byte)) (Bytes.of_string part))
      parts
  in
  Bytes.cat (Bytes.concat Bytes.empty encoded_parts) (Bytes.of_string "\x00")
;;

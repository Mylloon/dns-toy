open Types
open Utils

let header_to_bytes header =
  new_buffer
    (2 * get_obj_size header)
    [ header.id
    ; header.flags
    ; header.num_questions
    ; header.num_answers
    ; header.num_authorities
    ; header.num_additionals
    ]
;;

let question_to_bytes question =
  let buffer =
    new_buffer (2 * (get_obj_size question - 1)) [ question.type_; question.class_ ]
  in
  Bytes.cat question.name buffer
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

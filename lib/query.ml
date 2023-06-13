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

let question_to_bytes (question : dns_question) =
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

let build ?(id = None) domain_name record_type =
  let class_IN = 1 in
  let header =
    { id =
        (match id with
         | Some i -> i
         | None -> Random.int 65535)
    ; flags = 1 lsl 8
    ; num_questions = 1
    ; num_answers = 0
    ; num_authorities = 0
    ; num_additionals = 0
    }
  in
  let question =
    { name = encode_dns_name domain_name; type_ = record_type; class_ = class_IN }
  in
  Bytes.cat (header_to_bytes header) (question_to_bytes question)
;;

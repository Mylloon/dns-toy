type dns_header =
  { id : int
  ; flags : int
  ; num_questions : int
  ; num_answers : int
  ; num_authorities : int
  ; num_additionals : int
  }

type dns_question =
  { name : bytes
  ; type_ : int
  ; class_ : int
  }

let get_bytecode data =
  let result =
    List.map
      (fun byte ->
        let code = Char.code byte in
        if code >= 65 && code <= 122
        then String.make 1 byte
        else Printf.sprintf "\\x%02X" code)
      (List.of_seq (Bytes.to_seq data))
  in
  String.concat "" result
;;

let new_buffer obj list =
  let size = 2 * Obj.size (Obj.repr obj) in
  let buffer = Bytes.create size in
  let verification =
    List.fold_left
      (fun acc field ->
        Bytes.set_uint16_be buffer acc field;
        acc + 2)
      0
      list
  in
  if verification != size then failwith "Issue converting header to bytes";
  buffer
;;

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

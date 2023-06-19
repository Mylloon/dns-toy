let dns_header (header : Types.dns_header) =
  Printf.sprintf
    "{ \"id\": %d, \"flags\": %d, \"num_questions\": %d, \"num_answers\": %d, \
     \"num_authorities\": %d, \"num_additionals\": %d }"
    header.id
    header.flags
    header.num_questions
    header.num_answers
    header.num_authorities
    header.num_additionals
;;

let dns_question (question : Types.dns_question) =
  Printf.sprintf
    "{ \"name\": \"%s\", \"type_\": %d, \"class_\": %d }"
    (Bytes.to_string question.name)
    question.type_
    question.class_
;;

let dns_record (record : Types.dns_record) =
  Printf.sprintf
    "{ \"name\": \"%s\", \"type_\": %d, \"class_\": %d, \"ttl\": %d, \"data\": \"%s\" }"
    (Bytes.to_string record.name)
    record.type_
    record.class_
    record.ttl
    (Utils.get_bytecode ~json:true record.data)
;;

let dns_packet (record : Types.dns_packet) =
  let list fn l = String.concat ", " (List.map (fun el -> fn el) l) in
  Printf.sprintf
    "{ \"header\": %s, \"questions\": [%s], \"answers\": [%s], \"authorities\": [%s], \
     \"additionals\": [%s] }"
    (dns_header record.header)
    (list dns_question record.questions)
    (list dns_record record.answers)
    (list dns_record record.authorities)
    (list dns_record record.additionals)
;;

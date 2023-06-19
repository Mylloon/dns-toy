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

let dns_record ?(json = false) (record : Types.dns_record) =
  Printf.sprintf
    "{ \"name\": \"%s\", \"type_\": %d, \"class_\": %d, \"ttl\": %d, \"data\": \"%s\" }"
    (Bytes.to_string record.name)
    record.type_
    record.class_
    record.ttl
    (if Bytes.length record.data = 4
     then Utils.get_ip record.data
     else Utils.get_bytecode ~json record.data)
;;

let dns_packet (record : Types.dns_packet) =
  let list l =
    String.concat
      ", "
      (match l with
       | `Question lq -> List.map (fun el -> dns_question el) lq
       | `Record lr -> List.map (fun el -> dns_record ~json:true el) lr)
  in
  Printf.sprintf
    "{ \"header\": %s, \"questions\": [%s], \"answers\": [%s], \"authorities\": [%s], \
     \"additionals\": [%s] }"
    (dns_header record.header)
    (list (`Question record.questions))
    (list (`Record record.answers))
    (list (`Record record.authorities))
    (list (`Record record.additionals))
;;

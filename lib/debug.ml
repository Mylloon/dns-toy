let print_dns_header (header : Types.dns_header) =
  Printf.printf
    "{ id = %d; flags = %d; num_questions = %d; num_answers = %d; num_authorities = %d; \
     num_additionals = %d }\n"
    header.id
    header.flags
    header.num_questions
    header.num_answers
    header.num_authorities
    header.num_additionals
;;

let print_dns_question (question : Types.dns_question) =
  Printf.printf
    "{ name = '%s'; type_ = %d; class_ = %d }\n"
    (Bytes.to_string question.name)
    question.type_
    question.class_
;;

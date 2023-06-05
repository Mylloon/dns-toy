open Dnstoy.Utils

let () =
  let data =
    { id = 0x1314
    ; flags = 0
    ; num_questions = 1
    ; num_answers = 0
    ; num_authorities = 0
    ; num_additionals = 0
    }
  in
  assert (
    "\\x13\\x14\\x00\\x00\\x00\\x01\\x00\\x00\\x00\\x00\\x00\\x00"
    = get_bytecode (header_to_bytes data));
  assert ("\\x06google\\x03com\\x00" = get_bytecode (encode_dns_name "google.com"))
;;

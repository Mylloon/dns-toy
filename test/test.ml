open Dnstoy

let () =
  let data : Types.dns_header =
    { id = 0x1314
    ; flags = 0
    ; num_questions = 1
    ; num_answers = 0
    ; num_authorities = 0
    ; num_additionals = 0
    }
  in
  (* Header *)
  assert (
    "\\x13\\x14\\x00\\x00\\x00\\x01\\x00\\x00\\x00\\x00\\x00\\x00"
    = Utils.get_bytecode (Query.header_to_bytes data));
  (* Dns name *)
  let dns_name = Query.encode_dns_name "google.com" in
  assert ("\\x06google\\x03com\\x00" = Utils.get_bytecode dns_name);
  assert (6 = Char.code (Bytes.get dns_name 0))
;;

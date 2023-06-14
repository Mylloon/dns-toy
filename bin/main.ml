open Dnstoy

let () =
  let response = Network.send_request "8.8.8.8" "www.example.com" in
  print_endline (Utils.get_bytecode response);
  let response', dns_header = Response.parse_header response in
  let response'', dns_question = Response.parse_question response' in
  let dns_record = Response.parse_record response'' in
  Debug.print_dns_header dns_header;
  Debug.print_dns_question dns_question;
  Debug.print_dns_record dns_record
;;

open Dnstoy

let () =
  let response = Network.send_request "8.8.8.8" "www.example.com" in
  print_endline (Utils.get_bytecode response);
  let reader, dns_header = Response.parse_header { data = response; pointer = 0 } in
  let reader', dns_question = Response.parse_question reader in
  let dns_record = Response.parse_record reader' in
  Debug.print_dns_header dns_header;
  Debug.print_dns_question dns_question;
  Debug.print_dns_record dns_record
;;

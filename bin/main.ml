open Dnstoy

let () =
  let response = Network.send_request "8.8.8.8" "www.example.com" in
  print_endline (Utils.get_bytecode response);
  let dns_packet = Response.parse_dns_packet response in
  print_endline (Debug.dns_packet dns_packet);
  print_endline (Network.lookup_domain "example.com");
  print_endline (Network.lookup_domain "recurse.com");
  print_endline (Network.lookup_domain "www.metafilter.com")
;;

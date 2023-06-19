open Dnstoy

let () =
  let response = Network.send_request "8.8.8.8" "www.mylloon.fr" in
  print_endline (Utils.get_bytecode response);
  let dns_packet = Response.parse_dns_packet response in
  print_endline (Debug.dns_packet dns_packet)
;;

open Dnstoy

let print = function
  | Some value -> print_endline (Utils.get_bytecode value)
  | None -> print_endline "No IP found"
;;

let () =
  let response = Network.send_request "8.8.8.8" "www.mylloon.fr" Types.DNSType.a in
  print_endline (Utils.get_bytecode response);
  let dns_packet = Response.parse_dns_packet response in
  print_endline (Debug.dns_packet dns_packet);
  print (Network.lookup_domain "example.com");
  print (Network.lookup_domain "recurse.com");
  print (Network.lookup_domain "www.metafilter.com");
  List.iter
    (fun e -> print_endline (Debug.dns_record e))
    (Network.send "8.8.8.8" "example.com" 16).answers
;;

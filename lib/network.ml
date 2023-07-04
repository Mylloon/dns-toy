open Unix
open Query

let send_request dns url record_type =
  let query = build url record_type in
  let sock = socket PF_INET SOCK_DGRAM 0 in
  let server_addr = ADDR_INET (inet_addr_of_string dns, 53) in
  ignore (sendto sock query 0 (Bytes.length query) [] server_addr);
  let buffer_size = 1024 in
  let response = Bytes.create buffer_size in
  let len, _ = recvfrom sock response 0 buffer_size [] in
  Bytes.sub response 0 len
;;

let lookup_domain domain =
  let response = send_request "8.8.8.8" domain Types.DNSType.a in
  let dns_packet = Response.parse_dns_packet response in
  if dns_packet.header.num_answers > 0
  then Some (List.nth dns_packet.answers 0).data
  else None
;;

let send ip_address domain_name record_type =
  let query = send_request ip_address domain_name record_type in
  Response.parse_dns_packet query
;;

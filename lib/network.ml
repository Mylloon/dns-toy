open Unix
open Query
open Types

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

let rec resolve domain_name record_type = resolve_aux "198.41.0.4" domain_name record_type

and resolve_aux nameserver domain_name record_type =
  Printf.printf "Querying %s for %s\n" nameserver domain_name;
  let response = send nameserver domain_name record_type in
  match get_answer response with
  | Some ip -> String.of_bytes ip
  | None ->
    (match get_nameserver_ip response with
     | Some ns_ip -> resolve_aux ns_ip domain_name record_type
     | None ->
       (match get_nameserver response with
        | Some ns_domain ->
          resolve_aux (resolve ns_domain DNSType.a) domain_name record_type
        | None -> raise (Failure "Something went wrong")))

and get_answer packet =
  match List.find_opt (fun el -> el.type_ = DNSType.a) packet.answers with
  | Some record -> Some record.data
  | None -> None

and get_nameserver_ip packet =
  match List.find_opt (fun el -> el.type_ = DNSType.a) packet.additionals with
  | Some record -> Some (String.of_bytes record.data)
  | None -> None

and get_nameserver packet =
  match List.find_opt (fun el -> el.type_ = DNSType.ns) packet.authorities with
  | Some record -> Some (String.of_bytes record.data)
  | None -> None
;;

open Unix
open Query

let send_request dns url =
  let query = build url 1 in
  let sock = socket PF_INET SOCK_DGRAM 0 in
  let server_addr = ADDR_INET (inet_addr_of_string dns, 53) in
  ignore (sendto sock query 0 (Bytes.length query) [] server_addr);
  let buffer_size = 1024 in
  let response = Bytes.create buffer_size in
  let len, _ = recvfrom sock response 0 buffer_size [] in
  Bytes.sub response 0 len
;;

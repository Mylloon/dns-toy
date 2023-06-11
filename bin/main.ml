open Dnstoy

let () =
  print_endline (Utils.get_bytecode (Network.send_request "8.8.8.8" "www.example.com"))
;;

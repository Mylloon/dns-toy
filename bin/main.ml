open Dnstoy

let () = print_endline (Network.resolve "www.facebook.com" Types.DNSType.a)

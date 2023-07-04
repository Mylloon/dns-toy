open Dnstoy

let () = print_endline (Network.resolve "www.mylloon.fr" Types.DNSType.a)

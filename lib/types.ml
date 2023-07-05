type reader =
  { data : bytes
  ; pointer : int
  }

type dns_header =
  { id : int
  ; flags : int
  ; num_questions : int
  ; num_answers : int
  ; num_authorities : int
  ; num_additionals : int
  }

type dns_question =
  { name : bytes
  ; type_ : int
  ; class_ : int
  }

type dns_record =
  { name : bytes
  ; type_ : int
  ; class_ : int
  ; ttl : int
  ; data : bytes
  }

type dns_packet =
  { header : dns_header
  ; questions : dns_question list
  ; answers : dns_record list
  ; authorities : dns_record list
  ; additionals : dns_record list
  }

module DNSType = struct
  let a = 1
  let ns = 2
  let cname = 5
  let soa = 6
  let txt = 16
end

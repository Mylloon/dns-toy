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

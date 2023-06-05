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

open Types
open Utils

let parse_header reader =
  match List.init 6 (fun offset -> unpack_short_be reader (offset * 2)) with
  | [ id; flags; num_questions; num_answers; num_authorities; num_additionals ] ->
    { id; flags; num_questions; num_answers; num_authorities; num_additionals }
  | _ -> failwith "Invalid number of fields"
;;

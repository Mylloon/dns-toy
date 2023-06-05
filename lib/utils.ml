let get_bytecode data =
  let result =
    List.map
      (fun byte ->
        let code = Char.code byte in
        if code >= 65 && code <= 122
        then String.make 1 byte
        else Printf.sprintf "\\x%02X" code)
      (List.of_seq (Bytes.to_seq data))
  in
  String.concat "" result
;;

let new_buffer obj list =
  let size = 2 * Obj.size (Obj.repr obj) in
  let buffer = Bytes.create size in
  let verification =
    List.fold_left
      (fun acc field ->
        Bytes.set_uint16_be buffer acc field;
        acc + 2)
      0
      list
  in
  if verification != size then failwith "Issue converting header to bytes";
  buffer
;;

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

let get_obj_size obj = Obj.size (Obj.repr obj)

let new_buffer size list =
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

let unpack_short_be data offset =
  let msb = int_of_char (Bytes.get data offset) in
  let lsb = int_of_char (Bytes.get data (offset + 1)) in
  (msb lsl 8) + lsb
;;

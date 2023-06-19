let get_bytecode ?(json = false) data =
  let backslash = if json then "\\\\" else "\\" in
  let result =
    List.map
      (fun byte ->
        let code = Char.code byte in
        if code >= 65 && code <= 122
        then String.make 1 byte
        else Printf.sprintf "%sx%02X" backslash code)
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

let unpack_int_be data offset =
  let byte1 = int_of_char (Bytes.get data offset) in
  let byte2 = int_of_char (Bytes.get data (offset + 1)) in
  let byte3 = int_of_char (Bytes.get data (offset + 2)) in
  let byte4 = int_of_char (Bytes.get data (offset + 3)) in
  (byte1 lsl 24) lor (byte2 lsl 16) lor (byte3 lsl 8) lor byte4
;;

let bytes_forward data offset = Bytes.sub data offset (Bytes.length data - offset)

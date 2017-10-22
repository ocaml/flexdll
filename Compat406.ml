(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

module Buffer = struct
  include Buffer

  (* Taken from 4.06.0 *)
  let add_utf_16le_uchar b u = match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0xFFFF ->
      Buffer.add_char b (Char.unsafe_chr (u land 0xFF));
      Buffer.add_char b (Char.unsafe_chr (u lsr 8))
  | u when u <= 0x10FFFF ->
      let u' = u - 0x10000 in
      let hi = 0xD800 lor (u' lsr 10) in
      let lo = 0xDC00 lor (u' land 0x3FF) in
      Buffer.add_char b (Char.unsafe_chr (hi land 0xFF));
      Buffer.add_char b (Char.unsafe_chr (hi lsr 8));
      Buffer.add_char b (Char.unsafe_chr (lo land 0xFF));
      Buffer.add_char b (Char.unsafe_chr (lo lsr 8))
  | _ -> assert false
end

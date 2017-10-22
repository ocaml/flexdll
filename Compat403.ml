(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

module Char = struct
  include Char

  (* Taken from 4.03.0 *)
  let lowercase_ascii c =
    if (c >= 'A' && c <= 'Z')
    then unsafe_chr(code c + 32)
    else c

  let uppercase_ascii c =
    if (c >= 'a' && c <= 'z')
    then unsafe_chr(code c - 32)
    else c
end

module String = struct
  include String

  let (lowercase_ascii, uppercase_ascii) =
    (* Taken from 4.03.0 (not available before 4.00.0) *)
    let map f s =
      let l = length s in
        if l = 0 then s else begin
          (* create and unsafe_set trigger an irrelevant deprecation warning on 4.02.x *)
          let r = create l in
          for i = 0 to l - 1 do unsafe_set r i (f (unsafe_get s i)) done;
          r
        end
    in
      (map Char.lowercase_ascii, map Char.uppercase_ascii)
end

module Uchar = struct
  let unsafe_of_int c = c

  let to_int c = c
end

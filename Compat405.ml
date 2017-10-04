(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

module Arg = struct
  include Arg

  let trim_cr s =
    let len = String.length s in
    if len > 0 && String.get s (len - 1) = '\r' then
      String.sub s 0 (len - 1)
    else
    s
  (* Taken from 4.05.0 (not available before 4.05.0) *)
  let read_aux trim sep file =
  let ic = open_in_bin file in
  let buf = Buffer.create 200 in
  let words = ref [] in
  let stash () =
    let word =  (Buffer.contents buf) in
    let word = if trim then trim_cr word else word in
    words := word :: !words;
    Buffer.clear buf
  in
  let rec read () =
    try
      let c = input_char ic in
      if c = sep then begin
        stash (); read ()
      end else begin
        Buffer.add_char buf c; read ()
      end
    with End_of_file ->
      if Buffer.length buf > 0 then
        stash () in
  read ();
  close_in ic;
  Array.of_list (List.rev !words)

  let read_arg = read_aux true '\n'

  let read_arg0 = read_aux false '\x00'

end

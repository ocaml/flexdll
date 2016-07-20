(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

(* Back-port required functionality from Bytes in 4.02.0 *)
type bytes = string
module Bytes = struct
  include String

  let blit_string = blit
  let sub_string = sub
  let of_string x = x
  let to_string x = x
  let cat = (^)
end
let output_bytes = output_string

module Buffer = struct
  include Buffer

  let to_bytes = contents
end

(* Introduced in 4.01.0 *)
let ( |> ) x f = f x

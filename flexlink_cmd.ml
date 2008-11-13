(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

(* A minimal replacement for cmd.exe (does not support any expansion;
   supports only a redirection of stdout).

   This is intended to lift the stupid limitation of cmd.exe's command-line
   length (8Kb, wherease Windows support 32Kb).
*)

open Unix

let rec redirect = function
  | ">" :: fn :: rest ->
      Some fn, rest
  | x :: rest ->
      let out, rest = redirect rest in
      out, x :: rest
  | [] ->
      None, []

let cmd_exe () =
  let argv = Sys.argv in
(*
  Array.iteri
    (fun i s -> Printf.printf "argv[%i] = %S\n%!" i s)
    argv;
*)

  match Array.to_list argv with
  | _ :: "/c" ::  prog :: l ->
      let out, l = redirect l in
      let newstdout =
        match out with
        | None -> stdout
        | Some r -> openfile r [O_WRONLY; O_CREAT; O_TRUNC] 0o777
      in
      let pid =
        create_process
          prog
          (Array.of_list (prog :: l))
          stdin
          newstdout
          stderr
      in
      let _, ret = waitpid [] pid in
      if out <> None then close newstdout;
      begin match ret with
      | WEXITED n -> exit n
      | _ -> exit 2
      end
  | _ ->
      exit 2


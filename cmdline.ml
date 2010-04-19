(************************************************************************)
(*   FlexDLL                                                            *)
(*   Alain Frisch                                                       *)
(*                                                                      *)
(*   Copyright 2007 Institut National de Recherche en Informatique et   *)
(*   en Automatique.                                                    *)
(************************************************************************)

let underscore = ref true
    (* Are "normal" symbols prefixed with an underscore? *)

let machine : [ `x86 | `x64 ] ref = ref `x86

let noexport = ref false
let custom_crt = ref false
let reexport_from_implibs = ref true
let use_default_libs = ref true
let subsystem = ref "console"
let explain = ref false
let builtin_linker = ref false
let toolchain : [ `MSVC | `MINGW | `CYGWIN | `LIGHTLD ] ref = ref `MSVC
let save_temps = ref false
let show_exports = ref false
let show_imports = ref false
let dry_mode = ref false
let verbose = ref 0
let dirs = ref []
let no_merge_manifest = ref false
let merge_manifest = ref false
let real_manifest = ref true
let add_flexdll_obj = ref true
let files = ref []
let exts = ref []
let output_file = ref ""
let exe_mode : [`DLL | `EXE | `MAINDLL] ref = ref `DLL
let extra_args = ref []
let dump_mode = ref false
let defexports = ref []
let noentry = ref false
let use_cygpath = ref true
let cygpath_arg : [`Yes | `No | `None] ref = ref `None
let implib = ref false
let deffile = ref None

let usage_msg =
  Printf.sprintf
    "FlexDLL version %s\n\nUsage:\n  flexlink -o <result.dll> file1.obj file2.obj ... -- <extra linker arguments>\n"
    Version.version

let footer =
  "\
Notes:
* The -I, -l and -L options do not need to be separated from their argument.
* An option like /linkXXX is an abbrevation for '-link XXX'.
* FlexDLL's object files are searched by default in the same directory as
  flexlink, or in the directory given by the environment variable FLEXDIR
  if it is defined.
* Extra argument can be passed in the environment variable FLEXLINKFLAGS.

Homepage: http://alain.frisch.fr/flexdll.html"

let specs = [
  "-o", Arg.Set_string output_file,
  " Choose the name of the output file";

  "-exe", Arg.Unit (fun () -> exe_mode := `EXE),
  " Link the main program as an exe file";

  "-maindll", Arg.Unit (fun () -> exe_mode := `MAINDLL),
  " Link the main program as a dll file";

  "-noflexdllobj", Arg.Clear add_flexdll_obj,
  " Do not add the Flexdll runtime object (for exe)";

  "-noentry", Arg.Set noentry,
  " Do not use the Flexdll entry point (for dll)";

  "-noexport", Arg.Set noexport,
  " Do not export any symbol";

  "-I", Arg.String (fun dir -> dirs := dir :: !dirs),
  "<dir> Add a directory where to search for files";

  "-L", Arg.String (fun dir -> dirs := dir :: !dirs),
  "<dir> Add a directory where to search for files";

  "-l", Arg.String (fun s -> files := ("-l" ^ s) :: !files),
  "<lib> Library file";

  "-chain", Arg.Symbol (["msvc";"cygwin";"mingw";"ld"],
			(function
			   | "msvc" -> toolchain := `MSVC
			   | "cygwin" -> toolchain := `CYGWIN
			   | "mingw" -> toolchain := `MINGW
                           | "ld" -> toolchain := `LIGHTLD
			   | _ -> assert false)),
  " Choose which linker to use";

  "-defaultlib", Arg.String (fun s -> exts := s :: !exts),
  "<obj> External object (no export, no import)";

  "-save-temps", Arg.Set save_temps,
  " Do not delete intermediate files";

  "-implib", Arg.Set implib,
  " Do not delete the generated import library";

  "-outdef", Arg.String (fun s -> deffile := Some s),
  " Produce a def file with exported symbols";

  "-v", Arg.Unit (fun () -> incr verbose),
  " Increment verbosity (can be repeated)";

  "-show-exports", Arg.Set show_exports,
  " Show exported symbols";

  "-show-imports", Arg.Set show_imports,
  " Show imported symbols";

  "-dry", Arg.Set dry_mode,
  " Show the linker command line, do not actually run it";

  "-dump", Arg.Set dump_mode,
  " Only dump the content of object files";

  "-nocygpath", Arg.Unit (fun () -> cygpath_arg := `No),
  " Do not use cygpath (default for msvc, mingw)";

  "-cygpath", Arg.Unit (fun () -> cygpath_arg := `Yes),
  " Use cygpath (default for cygwin)";

  "-no-merge-manifest", Arg.Set no_merge_manifest,
  " Do not merge the manifest (takes precedence over -merge-manifest)";

  "-merge-manifest", Arg.Set merge_manifest,
  " Merge manifest to the dll or exe (if generated)";

  "-real-manifest", Arg.Set real_manifest,
  " Use the generated manifest (default behavior)";

  "-default-manifest", Arg.Clear real_manifest,
  " Use the default manifest";

  "-export", Arg.String (fun s -> defexports := s :: !defexports),
  "<sym> Explicitly export a symbol";

  "-noreexport", Arg.Clear reexport_from_implibs,
  " Do not reexport symbols imported from import libraries";

  "-where", Arg.Unit
    (fun () ->
      print_endline (Filename.dirname Sys.executable_name);
      exit 0
    ),
  " Show the FlexDLL directory";

  "-nounderscore", Arg.Clear underscore,
  " Normal symbols are not prefixed with an underscore";

  "-nodefaultlibs", Arg.Clear use_default_libs,
  " Do not assume any default library";

  "-builtin", Arg.Set builtin_linker,
  " Use built-in linker to produce a dll";

  "-x64", Arg.Unit (fun () -> machine := `x64; underscore := false),
  " x86_64 mode";

  "-explain", Arg.Set explain,
  " Explain why library objects are linked";

  "-subsystem", Arg.Set_string subsystem,
  "<id> Set the subsystem (default: console)";

  "-custom-crt", Arg.Set custom_crt,
  " Use a custom CRT";

  "-link", Arg.String (fun s -> extra_args := s :: !extra_args),
  "<option> Next argument is passed verbatim to the linker";

  "-D", Arg.String (fun _ -> ()),
  "<symbol> (Ignored)";

  "-U", Arg.String (fun _ -> ()),
  "<symbol> (Ignored)";

  "--", Arg.Rest (fun s -> extra_args := s :: !extra_args),
  " Following arguments are passed verbatim to the linker";
]


let flexlinkflags =
  let s =
    try Sys.getenv "FLEXLINKFLAGS"
    with Not_found -> ""
  in
  let n = String.length s in
  let rec skip_ws i = if i < n && s.[i] = ' ' then skip_ws (i + 1) else i in
  let rec scan_quote i = if i = n then i else if s.[i] = '"' then i + 1 else scan_quote (i+1) in
  let rec scan_arg i =
    if i = n || s.[i] = ' ' then i
    else if s.[i] = '"' then scan_arg (scan_quote (i + 1))
    else scan_arg (i + 1) in
  let rec args i =
    let i = skip_ws i in
    if i = n then []
    else let j = scan_arg i in String.sub s i (j - i) :: args j
  in
  args 0

let parse_cmdline () =
  (* Split -lXXX, -LXXX and -IXXX options *)
  let tosplit = function
    | "-l" | "-L" | "-I" | "-D" | "-U" -> true
    | _ -> false
  in

  let rec tr = function
    | (("-defaultlib"|"-link") as d) :: x :: rest -> d :: x :: tr rest
    | "/link" :: x :: rest -> "-link" :: x :: tr rest
    | s :: rest when String.length s > 2 && tosplit (String.sub s 0 2) ->
        String.sub s 0 2 :: String.sub s 2 (String.length s - 2) :: tr rest
    | s :: rest when String.length s >= 5 && String.sub s 0 5 = "/link" ->
        "-link" :: String.sub s 5 (String.length s - 5) :: tr rest
    | x :: rest when x <> "" && x.[0] = '-' ->
        begin
          try
            let i = String.index x ':' in
            String.sub x 0 i :: String.sub x (i + 1) (String.length x - i - 1)
            :: tr rest
          with Not_found ->
            x :: tr rest
        end
    | x :: rest -> x :: tr rest
    | [] -> []
  in
  let args =
    match Array.to_list Sys.argv with
    | pgm :: args -> pgm :: tr (flexlinkflags @ args)
    | _ -> assert false
  in

  Arg.parse_argv (Array.of_list args) (Arg.align specs)
    (fun x -> files := x :: !files) usage_msg;
  if !output_file = "" && not !dump_mode then begin
    Printf.eprintf
      "Please specify an output file (-help to get some usage information)\n";
    exit 1
  end

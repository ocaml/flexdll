open Coff

let toolchain = ref `MSVC
let save_temps = ref false
let show_exports = ref false
let show_imports = ref false
let temps = ref []
let verbose = ref 0
let search_path = ref []
let default_libs = ref []


let int32_to_buf b i =
  Buffer.add_char b (Char.chr (i land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff))

let drop_underscore s =
  assert(s.[0] = '_');
  String.sub s 1 (String.length s - 1)

let has_prefix pr s =
  String.length s > String.length pr && String.sub s 0 (String.length pr) = pr

let check_prefix pr s =
  if has_prefix pr s then
    Some (String.sub s (String.length pr) (String.length s - String.length pr))
  else None

let parse_libpath s =
  let n = String.length s in
  let rec aux l = 
    if l >= n then []
    else 
      try
	let i = String.index_from s l ';' in
	String.sub s l (i - l) :: aux (succ i)
      with Not_found -> [ String.sub s l (n - l) ]
  in
  aux 0

let getcmd cmd =
  if (Sys.command (cmd ^ " > tmp_getcmd") < 0) 
  then failwith ("Cannot run " ^ cmd);
  let ic = open_in "tmp_getcmd" in
  let s = input_line ic in
  close_in ic;
  Sys.remove "tmp_getcmd";
  s

let find_file fn =
  if Sys.file_exists fn then fn
  else 
    Filename.concat
      (List.find (fun s -> Sys.file_exists (Filename.concat s fn)) 
	 !search_path)
      fn
      
let find_file =
  let memo = Hashtbl.create 16 in
  fun fn ->
    try Hashtbl.find memo fn
    with Not_found ->
      let fn = 
	if String.length fn > 2 && String.sub fn 0 2 = "-l" then
	  "lib" ^ (String.sub fn 2 (String.length fn - 2))
	else fn in
      let r =
	try find_file fn
	with Not_found ->
	  try find_file (fn ^ ".lib")
	  with Not_found -> 
	    try find_file (fn ^ ".a")
	    with Not_found -> 
	      failwith (Printf.sprintf "Cannot find file %S" fn)
      in
      Hashtbl.add memo fn r;
      r

module StrSet = Set.Make(String)
    
(* Put all the relocations on the symbols defined by a predicate
   into a relocation table. A relocation table describes how to
   patch some addresses with the value of some external symbols (given
   by their name). It also lists segments that are normally write-protected
   and that must be de-protected to enable the patching process. *)

let add_reloc_table x p sname =
  let sect = Section.create ".reltbl" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let nonwr = ref [] in
  let nonwrsym = Symbol.intern sect 0l in
  let strsym = Symbol.intern sect 0l in
  let str_pos = Hashtbl.create 16 in

  Reloc.abs sect 0l nonwrsym;
  int32_to_buf data 0;

  (* TODO: use a single symbol per section *)
  let syms = ref [] in
  let reloc sec secsym min max rel =
    if p rel.symbol then (
      (* kind *)
      let kind = match rel.rtype with
	| 0x06 -> 0x0002 (* absolute *)
	| 0x14 -> 0x0001 (* relative *)
	| k    -> failwith (Printf.sprintf "Unsupported relocated kind %04x" k)
      in
      int32_to_buf data kind;

      (* name *)
      let name = drop_underscore (rel.symbol.sym_name) in
      let pos =
	try Hashtbl.find str_pos name
	with Not_found ->
	  let pos = Buffer.length strings in
	  Hashtbl.add str_pos name pos;
	  Buffer.add_string strings name;
	  Buffer.add_char strings '\000';
	  pos
      in
      Reloc.abs sect (Int32.of_int (Buffer.length data)) strsym;
      int32_to_buf data pos;

      Reloc.abs sect (Int32.of_int (Buffer.length data)) 
	(Lazy.force secsym);
      int32_to_buf data (Int32.to_int rel.addr);

      if rel.addr <= !min then min := rel.addr;
      if rel.addr >= !max then max := rel.addr;
      false
    ) else true
  in
  let section sec = 
    let min = ref Int32.max_int and max = ref Int32.min_int in
    let sym = lazy (let s = Symbol.intern sec 0l in
		    syms := s :: !syms;
		    s) in

    sec.relocs <- filter (reloc sec sym min max) sec.relocs;
    if (sec.sec_opts &&& 0x80000000l = 0l) && !min <= !max then
      nonwr := (!min,!max,Lazy.force sym) :: !nonwr
  in
  List.iter section x.sections;
  int32_to_buf data 0;
  strsym.value <- Int32.of_int (Buffer.length data);
  Buffer.add_buffer data strings;
  nonwrsym.value <- Int32.of_int (Buffer.length data);
  List.iter 
    (fun (min,max,secsym) ->
      Reloc.abs sect (Int32.of_int (Buffer.length data)) secsym;
      int32_to_buf data (Int32.to_int min);
      Reloc.abs sect (Int32.of_int (Buffer.length data)) secsym;
      int32_to_buf data (Int32.to_int  max);
      int32_to_buf data 0;
    )
    !nonwr;
  int32_to_buf data 0;
  int32_to_buf data 0;
  sect.data <- Buffer.contents data;
  x.sections <- sect :: x.sections;
  x.symbols <- 
    (Symbol.export sname sect 0l) ::
    strsym :: nonwrsym :: !syms @ List.filter (fun x -> not (p x)) x.symbols


(* Create a table for import symbols __imp_XXX *)

let add_import_table obj imports =
  let sect = Section.create ".imptbl" 0xc0300040l in
  obj.sections <- sect :: obj.sections;
  sect.data <- String.create (4 * List.length imports);
  ignore 
    (List.fold_left 
       (fun i s -> 
	  let sym = Symbol.extern s in
	  obj.symbols <- 
	    sym :: Symbol.export ("__imp_" ^ s) sect (Int32.of_int i) ::
	    obj.symbols;
	  Reloc.abs sect (Int32.of_int i) sym; i + 4)
       0 imports)


(* Create a table that lists exported symbols (adress,name) *)

let add_export_table obj exports symname =
  let sect = Section.create ".exptbl" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let strsym = Symbol.intern sect 0l in
  obj.symbols <- strsym :: (Symbol.export symname sect 0l) :: obj.symbols;
  let exports = List.sort Pervasives.compare exports in
  (* The runtime library assume the names are sorted! *)
  int32_to_buf data (List.length exports);
  List.iter
    (fun s  ->
       let sym = Symbol.extern s in
       obj.symbols <- sym :: obj.symbols;
       Reloc.abs sect (Int32.of_int (Buffer.length data)) sym;
       int32_to_buf data 0;

       Reloc.abs sect (Int32.of_int (Buffer.length data)) strsym;
       int32_to_buf data (Buffer.length strings);
       Buffer.add_string strings (drop_underscore s);
       Buffer.add_char strings '\000';
    )
    exports;
  strsym.value <- Int32.of_int (Buffer.length data);
  sect.data <- Buffer.contents data ^ Buffer.contents strings;
  obj.sections <- sect :: obj.sections

(* A master relocation table points to all the relocation tables
   in the DLL *)

let add_master_reloc_table obj names symname =
  let sect = Section.create ".mreltbl" 0xc0300040l in
  let data = Buffer.create 1024 in
  obj.symbols <- (Symbol.export symname sect 0l) :: obj.symbols;
  List.iter
    (fun s  ->
       let sym = Symbol.extern s in
       obj.symbols <- sym :: obj.symbols;
       Reloc.abs sect (Int32.of_int (Buffer.length data)) sym;
       int32_to_buf data 0;
    )
    names;
  int32_to_buf data 0;
  sect.data <- Buffer.contents data;
  obj.sections <- sect :: obj.sections



let collect_dllexports obj =
  let dirs = Coff.directives obj in
  List.map (function (_,x::_) -> x | _ -> assert false)
    (List.find_all (fun (cmd,args) -> String.uppercase cmd = "EXPORT") dirs)

let exports accu obj =
  List.fold_left 
    (fun accu sym -> 
       if Symbol.is_defin sym && sym.sym_name.[0] = '_' 
       then StrSet.add sym.sym_name accu
       else accu)
    accu obj.symbols

let needed f accu obj =
  let l = List.filter Symbol.is_extern obj.symbols in
  List.fold_left (fun accu sym -> StrSet.add (f sym.sym_name) accu) accu l

let collect f l =
  List.fold_left
    (fun accu x ->
       match f x with None -> accu | Some y -> y :: accu)
    []
    l

let build_dll link_exe output_file files extra_args =
  (* fully resolve filenames, eliminate duplicates *)
  let files = 
    StrSet.elements (
      List.fold_left (fun accu fn -> StrSet.add (find_file fn) accu)
	StrSet.empty files
    ) in
  
  (* load given files *)
  let loaded_filenames : (string,unit) Hashtbl.t = Hashtbl.create 16 in
  let files = List.map (fun fn -> fn, Lib.read fn) files in
  List.iter (fun (fn,_) -> Hashtbl.add loaded_filenames fn ()) files;

  let objs = 
    collect (function (f,`Obj x) -> Some (f,x) | _ -> None) files in
  let libs = 
    collect (function (f,`Lib (x,[])) -> Some (f,x) | _ -> None) files in
  let ilibs = 
    collect 
      (function (f,`Lib (x,imps)) when imps <> [] -> Some (f,x,imps) 
	 | _ -> None) files in

  let defined = ref StrSet.empty in
  if link_exe then defined := StrSet.add "_static_symtable" !defined;

  let aliases = Hashtbl.create 16 in
  let rec normalize name =
    try 
      let r = Hashtbl.find aliases name in
      if r <> name then normalize r else r
    with Not_found -> name in

  (* Collect all the available symbols, including those defined
     in default libraries *)
  let rec collect_defined_obj obj =
    List.iter (fun (x,y) -> Hashtbl.add aliases x y) (Coff.aliases obj);
    let dirs = Coff.directives obj in
    let all_args c =
      List.map snd (
	List.find_all (fun (cmd,args) -> String.uppercase cmd = c)
	  dirs)
    in
    let deflibs = List.flatten (all_args "DEFAULTLIB") in
    List.iter (fun fn -> 
		 let fn = find_file fn in
		 if not (Hashtbl.mem loaded_filenames fn)
		 then (Hashtbl.add loaded_filenames fn (); 
		       collect_defined (Lib.read fn)))
      deflibs;
    List.iter
      (fun sym -> 
	 if Symbol.is_defin sym 
	 then defined := StrSet.add sym.sym_name !defined;
      )
      obj.symbols 
  and collect_defined = function
    | `Obj obj -> collect_defined_obj obj
    | `Lib (objs,imports) -> 
	List.iter (fun (_,obj) -> collect_defined_obj obj) objs;
	List.iter
	  (fun (s,_) -> 
	     defined := StrSet.add s (StrSet.add ("__imp_" ^ s) !defined))
	  imports
  in
  List.iter (fun (_,x) -> collect_defined x) files;

  List.iter
    (fun fn -> collect_defined (Lib.read (find_file fn)))
    !default_libs;

  (* Determine which objects from the given libraries should be linked
     in. First step: find the mapping (symbol -> object) for these
     objects. *)
  let defined_in = Hashtbl.create 16 in
  let def_in_obj fn (objname,obj) = List.iter
    (fun sym -> 
       if Symbol.is_defin sym 
       then Hashtbl.replace defined_in sym.sym_name (fn,objname,obj);
    ) obj.symbols in
  List.iter
    (fun (fn,objs) -> List.iter (def_in_obj fn) objs)
    libs;
  
  let imported = ref StrSet.empty in
  let needed obj = needed normalize StrSet.empty obj in
  let imports obj = 
    StrSet.filter
      (fun s -> match check_prefix "__imp_" s with
	 | Some s' -> imported := StrSet.add s' !imported; false
	 | None -> true)
      (StrSet.diff (needed obj) !defined) in
  
  (* Second step: transitive closure, starting from given objects *)

  let libobjects  = Hashtbl.create 16 in
  let to_link = ref [] in
  let reloctbls = ref [] in
  let exported = ref StrSet.empty in

  let record_obj obj =
    let fn = Filename.temp_file "dyndll" ".obj" in
    to_link := fn :: !to_link;
    temps := fn :: !temps;
    let oc = open_out_bin fn in
    Coff.put oc obj;
    close_out oc in

  let add_reloc name obj imps =
    if !show_imports then (
      Printf.printf "** Imported symbols for %s:\n" name;
      StrSet.iter print_endline imps
    );
    let reloctbl = Symbol.gen_sym () in
    reloctbls := reloctbl :: !reloctbls;
    add_reloc_table obj (fun s -> StrSet.mem s.sym_name imps) reloctbl in

  let close_obj name imps obj = 
    if link_exe then
      failwith (Printf.sprintf "Cannot resolve symbols:\n %s\n"
		  (String.concat "\n " (StrSet.elements imps)));
    add_reloc name obj imps;
    record_obj obj in

  let rec link_obj obj = 
    exported := exports !exported obj;
    StrSet.iter 
      (fun s -> try link_libobj (Hashtbl.find defined_in s) with Not_found->())
      (needed obj)
  and link_libobj (libname,objname,obj) =
    if Hashtbl.mem libobjects (libname,objname) then ()
    else (Hashtbl.replace libobjects (libname,objname) (obj,imports obj); 
	  link_obj obj) in
  List.iter 
    (fun (fn,obj) -> 
       link_obj obj;
       let imps = imports obj in
       if (StrSet.is_empty imps) then to_link := fn :: !to_link
       else close_obj fn imps obj;
    ) objs;

  let to_explode = Hashtbl.create 16 in
  Hashtbl.iter
    (fun (libname,objname) (obj,imps) ->
       if not (StrSet.is_empty imps) then Hashtbl.replace to_explode libname ()
    )
    libobjects;
  let libs = ref StrSet.empty in
  Hashtbl.iter
    (fun (libname,objname) (obj,imps) ->
       if Hashtbl.mem to_explode libname 
       then close_obj (Printf.sprintf "%s(%s)" libname objname) imps obj
       else libs := StrSet.add libname !libs 
    )
    libobjects;
  StrSet.iter (fun l -> 
		 if not (Hashtbl.mem to_explode l) 
		 then to_link := l :: !to_link) 
    !libs;
  if !show_exports then (
    Printf.printf "** Exported symbols:\n";
    StrSet.iter print_endline !exported;
  );

  (* Create the descriptor object *)
  let obj = Coff.empty () in

  if not (StrSet.is_empty !imported) then begin
    add_import_table obj (StrSet.elements !imported);
    add_reloc "descriptor object" obj !imported;
  end;

  add_export_table obj (StrSet.elements !exported) 
    (if link_exe then "_static_symtable" else "_symtbl");
  if not link_exe then add_master_reloc_table obj !reloctbls "_reloctbl";
  record_obj obj;


  let files = !to_link @ List.map (fun (fn,_,_) -> fn) ilibs in
  let files = List.map Filename.quote files in
  let files = String.concat " " files in
  let quiet = if !verbose >= 1 then "" else ">NUL" in
  let cmd = match !toolchain with
    | `MSVC ->
	let implib = Filename.temp_file "dyndll_implib" ".lib" in
	let impexp = Filename.chop_suffix implib ".lib" ^ ".exp" in
	temps := implib :: impexp :: !temps;
	Printf.sprintf 
	  "link /nologo %s%s /implib:%s /out:%s %s %s%s"
	  (if !verbose >= 2 then "/verbose " else "")
	  (if link_exe then "" else "/dll /export:symtbl /export:reloctbl ")
	  (Filename.quote implib)
	  (Filename.quote output_file) files extra_args quiet
    | `CYGWIN ->
	Printf.sprintf
	  "gcc %s -L. -o %s %s %s"
	  (if link_exe then "" else "-shared ")
	  (Filename.quote output_file)
	  files
	  extra_args
    | `MINGW ->
	Printf.sprintf
	  "gcc -mno-cygwin %s -L. -o %s %s %s"
	  (if link_exe then "" else "-shared ")
	  (Filename.quote output_file)
	  files
	  extra_args
  in
  if !verbose >= 1 then Printf.printf "+ %s\n" cmd; 
  flush stdout;
  if Sys.command cmd <> 0 then failwith "Error during linking\n"


let files = ref []
let output_file = ref ""
let exe_mode = ref false  
let extra_args = ref []
let usage_msg = 
  "reloc -o <result.dll> file1.obj file2.obj ... -- <extra linker arguments>"
let specs = [
  "-o", Arg.Set_string output_file, 
  " Choose the name of the output file";

  "-exe", Arg.Set exe_mode,
  " Link an executable (not a dll)";

  "-chain", Arg.Symbol (["msvc";"cygwin";"mingw"],
			(function 
			   | "msvc" -> toolchain := `MSVC
			   | "cygwin" -> toolchain := `CYGWIN
			   | "mingw" -> toolchain := `MINGW
			   | _ -> assert false)),
  " Choose which linker to use";

  "-save-temps", Arg.Set save_temps,
  " Do not delete intermediate files";

  "-v", Arg.Unit (fun () -> incr verbose),
  " Increment verbosity (can be repeated)";

  "-show-exports", Arg.Set show_exports,
  " Show exported symbols";

  "-show-imports", Arg.Set show_imports,
  " Show imported symbols";

  "--", Arg.Rest (fun s -> extra_args := s :: !extra_args),
  " Introduce extra linker arguments";
]

let safe_remove s =
  try Sys.remove s
  with Sys_error _ -> ()

let clean () =
  if not !save_temps 
  then (List.iter safe_remove !temps; temps := [])

let setup_toolchain () = match !toolchain with
  | `CYGWIN -> 
      let cygroot = getcmd "cygpath -m /"
      and gcclib = getcmd "gcc -print-libgcc-file-name | cygpath -m -f -" in
      search_path := [ Filename.concat cygroot "lib";
		       Filename.concat cygroot "lib/w32api";
		       Filename.dirname gcclib ];
      default_libs := ["-lkernel32"; "-luser32"; "-ladvapi32";
		       "-lshell32"; "-lcygwin"; "-lgcc"]
  | `MSVC ->
      search_path := parse_libpath (try Sys.getenv "LIB" with Not_found -> "")
  | `MINGW ->
      let cygroot = getcmd "cygpath -m /"
      and gcclib = 
	getcmd "gcc -mno-cygwin -print-libgcc-file-name | cygpath -m -f -" in
      search_path := [ Filename.concat cygroot "lib";
		       Filename.concat cygroot "lib/mingw";
		       Filename.concat cygroot "lib/w32api";
		       Filename.dirname gcclib ];
      default_libs := 
	["-lmingw32"; "-lgcc"; "-lmoldname"; "-lmingwex"; "-lmsvcrt"; 
	 "-luser32"; "-lkernel32"; "-ladvapi32"; "-lshell32" ]

let () =
  at_exit clean;
  let specs = Arg.align specs in
  Arg.parse specs (fun x -> files := x :: !files) usage_msg;
  if !output_file = "" then 
    (Printf.eprintf "Please specify an output file\n"; 
     exit 1);
  try
    setup_toolchain ();
    if !verbose >= 2 then (
      Printf.printf "** Search path:\n";
      List.iter print_endline !search_path;
      Printf.printf "** Default libraries:\n";
      List.iter print_endline !default_libs;
    );
    build_dll !exe_mode !output_file !files 
      (String.concat " " (List.rev !extra_args));
  with 
    | Failure s ->
	Printf.eprintf "** Fatal error: %s\n" s;
	exit 2
    | exn ->
	Printf.eprintf "** Error: %s\n" (Printexc.to_string exn);
	exit 2

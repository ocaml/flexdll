(* Internal representation of COFF object files *)

type symbol = {
  mutable sym_pos: int;
  sym_name: string;
  mutable value: int32;
  mutable section: [ `Num of int | `Section of section ];
  stype: int;
  storage: int;
  auxn: int;
  auxs: string;
  mutable alias_for: symbol option;
}

and reloc = {
  addr: int32;
  symbol: symbol;
  rtype: int;
}

and section = {
  mutable sec_pos: int;
  sec_name: string;
  vsize: int32;
  mutable data: string;
  mutable relocs: reloc list;
  sec_opts: int32;
}

type coff = {
  machine: int;
  date: int32;
  mutable sections: section list;
  mutable symbols: symbol list;
  opts: int;
}


(* Misc *)


let (++) = Int32.add
let (&&&) = Int32.logand
let (>>>) = Int32.shift_right_logical

let filter f l =
  let rec aux accu = function
    | [] -> accu
    | hd::tl -> if f hd then aux (hd::accu) tl else aux accu tl
  in
  aux [] l


(* Tools to read/write binary data *)

let mk_int32 a b c d =
  let a = Int32.of_int a 
  and b = Int32.shift_left (Int32.of_int b) 8
  and c = Int32.shift_left (Int32.of_int c) 16
  and d = Int32.shift_left (Int32.of_int d) 24 in
  a ++ b ++ c ++ d

let read ic pos len =
  seek_in ic pos;
  let buf = String.create len in
  really_input ic buf 0 len;
  buf

let int32 buf loc =
  mk_int32 
    (Char.code buf.[loc]) (Char.code buf.[loc + 1])
    (Char.code buf.[loc + 2]) (Char.code buf.[loc + 3])

let emit_int32 oc i =
  output_byte oc (Int32.to_int (i &&& 0xffl));
  output_byte oc (Int32.to_int ((i >>> 8) &&& 0xffl));
  output_byte oc (Int32.to_int ((i >>> 16) &&& 0xffl));
  output_byte oc (Int32.to_int ((i >>> 24) &&& 0xffl))

let patch_int32 oc pos i =
  let bak = pos_out oc in
  seek_out oc pos;
  emit_int32 oc i;
  seek_out oc bak

let emit_int16 oc i =
  output_byte oc (i land 0xff);
  output_byte oc (i lsr 8)

let emit_int8 = output_byte

let int32_ buf loc = Int32.to_int (int32 buf loc)
  
let int16 buf loc = Char.code buf.[loc] + (Char.code buf.[loc + 1]) lsl 8

let int8 buf loc = Char.code buf.[loc] 
  
let strz buf loc ?(max=String.length buf - loc) c =
  let i = 
    try String.index_from buf loc c
    with Not_found -> String.length buf in
  String.sub buf loc (min max (i - loc))
    
let emit_zero oc n =
  for i = 1 to n do output_char oc '\000' done

let delayed_ptr oc f =
  let bak = pos_out oc in
  emit_int32 oc 0l;
  (fun () ->
     patch_int32 oc bak (Int32.of_int (pos_out oc));
     f ()
  )

(* Human readable pretty-printers *)

let flags x =
  let b = Buffer.create 16 in
  for i = 0 to 31 do
    let m = Int32.shift_left 1l i in
      if m &&& x <> 0l then
	Printf.bprintf b "0x%08lx " m
  done;
  Buffer.contents b
    
let rec dump ic pos len w =
  if len = 0 then (Printf.printf "---\n"; flush stdout)
  else let l = min len w in
  let b = read ic pos l in
  Printf.printf "%08x: " pos;
  for i = 0 to l - 1 do Printf.printf "%02x " (Char.code b.[i]) done;
  for i = l to w - 1 do Printf.printf "   " done;
  Printf.printf " ";
  for i = 0 to l - 1 do match b.[i] with
    | '\032'..'\127' as c -> print_char c
    | _ -> print_char '.'
  done;
  Printf.printf "\n";
  flush stdout;
  dump ic (pos + l) (len - l) w

module Symbol = struct
  let counter = ref 0
  let gen_sym () = incr counter; Printf.sprintf "DREL$%i" !counter

  let intern sec addr =
    { sym_pos = (-1); sym_name = gen_sym(); value = addr; 
      section = `Section sec; storage = 3; stype = 0; auxn = 0; auxs = "";
      alias_for = None }

  let export name sec addr = 
    { sym_pos = (-1); sym_name = name; value = addr;
      section = `Section sec; storage = 2; stype = 0; auxn = 0; auxs = "";
      alias_for = None }


  let get strtbl ic pos =
    let buf = read ic pos 18 in
    let auxn = int8 buf 17 in
    { sym_pos = (-1);
      sym_name = 	
	(if int32_ buf 0 <> 0 then strz buf 0 ~max:8 '\000'
	 else strtbl (int32_ buf 4));
      value = int32 buf 8;
      section = `Num (int16 buf 12);
      stype = int16 buf 14;
      storage = int8 buf 16;
      auxn = auxn;
      auxs = read ic (pos + 18) (18 * auxn);
      alias_for = None
    }

  let is_extern = function
    |  { storage = 2; section = `Num 0; value = 0l } -> true
    | _ -> false

  let is_export = function
    | { storage = 2; section = `Section _ } -> true
    | { storage = 2; section = `Num 0; value = 0l } -> false
    | { storage = 2; section = `Num 0 } -> true
    | _ -> false

  let is_defin = function
    | { storage = 2; section = `Section _ } -> true
    | { storage = 2; section = `Num 0; value = 0l } -> false
    | { storage = 2; section = `Num (0 | 0xffff) } -> true
    | _ -> false 

  let dump s = 
    Printf.printf " %s: " s.sym_name;
    if s.stype <> 0 then Printf.printf "(typ:%i) " s.stype;
    let sect = match s.section with 
      | `Num 0xffff -> "absolute"
      | `Num 0xfffe -> "debug"
      | `Num i -> string_of_int i
      | `Section s -> Printf.sprintf "%S" s.sec_name in
    let storage = match s.storage with
      | 2 -> "extern"
      | 3 -> "static"
      | 6 -> "label"
      | 103 -> "srcfile"
      | n -> string_of_int n in
    match s with
      | { storage = 6 } -> 
	  Printf.printf "label %s @ 0x%08lx\n" sect s.value
      | { storage = 2; section = `Section _ } ->
	  Printf.printf "export %s @ 0x%08lx\n" sect s.value
      | { storage = 2; section = `Num 0; value = 0l } ->
	  Printf.printf "extern\n"
      | { storage = 3 } ->
	  Printf.printf "static %s @ 0x%08lx\n" sect s.value
      | { storage = 103 } ->
	  Printf.printf "filename %s\n" (strz s.auxs 0 '\000')
      | { storage = 105 } ->
	  Printf.printf "weak ext\n"
      | _ ->
	  Printf.printf 
	    "value=0x%08lx, sect=%s, storage=%s, aux=%i\n"
	    s.value sect storage s.auxn

  let put strtbl oc s =
    if String.length s.sym_name <= 8 
    then (output_string oc s.sym_name; 
	  emit_zero oc (8 - String.length s.sym_name))
    else (emit_zero oc 4; emit_int32 oc (strtbl s.sym_name));
    emit_int32 oc s.value;
    let sec = match s.section with
      | `Num i -> i
      | `Section sec when sec.sec_pos <= 0 ->
	  Printf.eprintf "Cannot emit section for symbol %s" s.sym_name;
	  exit 1
      | `Section sec -> sec.sec_pos in
    emit_int16 oc sec;
    emit_int16 oc s.stype;
    emit_int8 oc s.storage;
    emit_int8 oc s.auxn;
    output_string oc s.auxs
end

module Reloc = struct
  let abs sec addr sym = 
    sec.relocs <- { addr = addr; symbol = sym; rtype = 6 } :: sec.relocs

  let get symtbl ic base =
    let buf = read ic base 10 in
    { addr = int32 buf 0;
      symbol = (try match symtbl.(int32_ buf 4) with Some s -> s 
		  | None -> assert false
		with exn -> assert false);
      rtype = int16 buf 8
    }

  let dump x =
    Printf.printf " Reloc %ld -> %s, type 0x%04x\n" 
      x.addr x.symbol.sym_name x.rtype

  let put oc x =
    emit_int32 oc x.addr;
    if x.symbol.sym_pos < 0 then (
      Printf.eprintf "Cannot emit relocation for symbol %s\n"
	x.symbol.sym_name;
      exit 2
    );
    emit_int32 oc (Int32.of_int x.symbol.sym_pos);
    emit_int16 oc x.rtype
end

module Section = struct
  let create name flags = {
    sec_pos = (-1); sec_name = name; data = ""; relocs = [];
    vsize = 0l;  sec_opts = flags;
  }

  let get filebase strtbl symtbl ic base =
    let buf = read ic base 40 in
    let size = int32_ buf 16 in
    let name = 
      if buf.[0] = '/' 
      then strtbl (int_of_string (strz buf 1 ~max:7 '\000'))
      else strz buf 0 ~max:8 '\000'
    in
    assert( int32 buf 12 = 0l );
    assert( int32 buf 28 = 0l );
    assert( int16 buf 34 = 0 );

    if (int32 buf 26 &&& 0x01000000l <> 0l) 
    then (Printf.printf "More relocs!\n"; assert false);


    let relocs =
      let base = filebase + int32_ buf 24 in
      let r = ref [] in
      for i = 0 to (int16 buf 32) - 1 do
	r := Reloc.get symtbl ic (base + 10 * i) :: !r
      done;
      !r
    in

    let data = read ic (filebase + int32_ buf 20) size in
(*    if name = ".drectve" then dump ic (filebase + int32_ buf 20) size 16; *)

    { sec_pos = (-1);
      sec_name = name;
      vsize = int32 buf 8;
      data = data;
      relocs = relocs;
      sec_opts = int32 buf 36
    }

  let dump x =
    Printf.printf "Section %s (0x%08lx: %s)\n"
      x.sec_name
      x.sec_opts
      (flags x.sec_opts);
    List.iter Reloc.dump x.relocs

  let put strtbl oc x =
    let name =
      if String.length x.sec_name <= 8 
      then x.sec_name
      else Printf.sprintf "/%ld" (strtbl x.sec_name)
    in
    output_string oc name; emit_zero oc (8 - String.length name);
    emit_int32 oc 0l;
    emit_int32 oc 0l;
    emit_int32 oc (Int32.of_int (String.length x.data));

    let send_data = delayed_ptr oc (fun () -> output_string oc x.data)
    in

    let send_reloc =
      if x.relocs = [] then (emit_int32 oc 0l; fun () -> ())
      else delayed_ptr oc (fun () -> List.iter (Reloc.put oc) x.relocs)
    in

    emit_int32 oc 0l;
    emit_int16 oc (List.length x.relocs);
    emit_int16 oc 0;
    emit_int32 oc x.sec_opts;

    send_data, send_reloc
end

module Coff = struct

  let get ic base =
    let buf = read ic base 20 in    
    let opthdr = int16 buf 16 in
    assert(opthdr = 0);

    let symtable = base + int32_ buf 8 in
    let symcount = int32_ buf 12 in

    (* the string table *)
    let strtbl = 
      let pos = symtable + 18 * symcount in
      let len = int32_ (read ic pos 4) 0 in
      let data = read ic pos len in
      fun i -> strz data i '\000'
    in

    (* the symbol table *)
    let symbols,symtbl = 
      let tbl = Array.create symcount None in
      let rec fill accu i =
	if i = symcount then accu
	else let s = Symbol.get strtbl ic (symtable + 18 * i) in
	tbl.(i) <- Some s;
	fill (s :: accu) (i + 1 + s.auxn) in
      fill [] 0, tbl
    in

    (* the sections *)
    let sectable = base + 20 + opthdr in
    let sections =
      Array.init (int16 buf 2)
	(fun i -> Section.get base strtbl symtbl ic (sectable + 40 * i))
    in
    List.iter
      (fun s ->
	 if s.storage = 105 then
	   s.alias_for <- symtbl.(Int32.to_int (int32 s.auxs 0));
	 match s.section with
	   | `Num i when i > 0 && i <= Array.length sections ->
	       assert (i <= Array.length sections);
	       s.section <- `Section sections.(i - 1)
	   | _ -> ())
      symbols;

    { machine = int16 buf 0;
      sections = Array.to_list sections;
      date = int32 buf 4;
      symbols = symbols;
      opts = int16 buf 18;
    }

  let aliases x =
    let a = ref [] in
    List.iter
      (fun s ->
	 match s.alias_for with 
	   | Some s' -> a := (s.sym_name,s'.sym_name) :: !a
	   | None -> ()
      )
      x.symbols;
    !a



  let dump x =
    Printf.printf "machine: 0x%x\n" x.machine;
    Printf.printf "date:    0x%lx\n" x.date;
    Printf.printf "opts:    0x%x\n" x.opts;
    List.iter Symbol.dump x.symbols;
    List.iter Section.dump x.sections

  let put oc x =
    emit_int16 oc x.machine;

    let () =
      let no = ref 0 in
      List.iter
	(fun s -> incr no; assert(s.sec_pos < 0); s.sec_pos <- !no)
	x.sections
    in

    emit_int16 oc (List.length x.sections);
    emit_int32 oc x.date;

    let strbuf = Buffer.create 1024 in
    let strtbl s =
      let pos = Buffer.length strbuf in
      Buffer.add_string strbuf s;
      Buffer.add_char strbuf '\000';
      Int32.of_int (4 + pos)
    in

    let patch_sym =
      delayed_ptr oc
	(fun () -> List.iter (Symbol.put strtbl oc) x.symbols)
    in
    let nbsym =
      let no = ref 0 in
      List.iter
	(fun s ->
	   assert(s.sym_pos < 0);
	   s.sym_pos <- !no;
	   no := !no + 1 + s.auxn
	)
	x.symbols;
      !no
    in
    emit_int32 oc (Int32.of_int nbsym);
    emit_int16 oc 0;
    emit_int16 oc x.opts;

    let sects = 
      List.map (Section.put strtbl oc) x.sections in
    List.iter
      (fun (data,relocs) -> data (); relocs ())
      sects;

    patch_sym ();

    emit_int32 oc (Int32.of_int (Buffer.length strbuf + 4));
    Buffer.output_buffer oc strbuf;

    List.iter (fun s -> s.sym_pos <- 0) x.symbols;
    List.iter (fun s -> s.sec_pos <- 0) x.sections;
    ()
end

module Import = struct
  let read ic pos size =
    let buf = read ic pos size in
    let w = int16 buf 18 in
    let name = strz buf 20 '\000' in
(*    Printf.printf "Import header. Version = %i\n" (int16 buf 4);
    Printf.printf " machine     = 0x%x\n" (int16 buf 6);
    Printf.printf " time stamp  = 0x%lx\n" (int32 buf 8);    
    Printf.printf " size data   = %ld\n" (int32 buf 12);
    Printf.printf " ord/hint    = %i\n" (int16 buf 16);
    Printf.printf " type        = %i\n" (w land 0b11);
    Printf.printf " name type   = %i\n" ((w land 0b11100) lsr 2); 
    Printf.printf " symbol      = %s\n" name;
    Printf.printf " DLL         = %s\n" 
      (strz buf (21 + String.length name) '\000'); *)
    name, w

end

module Lib = struct
  let magic_lib = "!<arch>\n"

  let read_lib ic =
    let strtbl = ref "" in
    let imports = ref [] and objects = ref [] in
    let obj size name = 
(*      Printf.printf "-> %s (size %i)\n" name size;  *)
      let pos = pos_in ic in
      if (size > 18) && (read ic pos 4 = "\000\000\255\255") 
      then imports := Import.read ic pos size :: !imports
      else objects := (name, Coff.get ic pos) :: !objects
    in
    let rec read_member () =
      let buf = read ic (pos_in ic) 60 in
      let base = pos_in ic in
      let size = int_of_string (strz (String.sub buf 48 10) 0 ' ') in
      let name = strz (String.sub buf 0 16) 0 ' '  in
      begin match name with
	| "/" | "" -> ()
	| "//" -> strtbl := read ic (pos_in ic) size
	| s when s.[0] = '/' ->
	    let ofs = int_of_string (String.sub s 1 (String.length s - 1)) in
	    obj size (strz !strtbl ofs '\000')
	| s ->
	    obj size (strz s 0 '/');
      end;
      seek_in ic (base + size + size mod 2); 
      read_member ()
    in
    (try read_member () with End_of_file -> ());
    !objects,!imports

  let read filename =
    let ic = open_in_bin filename in
    try
      let r = 
	if in_channel_length ic > String.length magic_lib
	  && read ic 0 (String.length magic_lib) = magic_lib 
	then `Lib (read_lib ic)
	else `Obj (Coff.get ic 0) in
      close_in ic;
      r
    with exn ->
      close_in ic;
      raise exn
end


(************************************************************************)

let remove_section x name =
  let sec = List.find (fun s -> s.sec_name = name) x.sections in
  { x with 
      sections = filter (fun s -> s != sec) x.sections;
      symbols = filter 
      (fun s -> match s.section with `Section se -> se != sec 
	 | _ -> true) x.symbols
  }

let remove_section_symbols x =
  { x with
      symbols = filter
      (fun s -> not (s.value = 0l && s.storage = 3 || s.storage = 103))
      x.symbols
  }

let int32_to_buf b i =
  Buffer.add_char b (Char.chr (i land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff))

let make_reloctable x p =
  let sect = Section.create ".dynrel" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let strsym = Symbol.intern sect 0l in

  (* TODO: share symbol names *)
  (* TODO: use a single symbol per section *)
  let syms = ref [] in
  let reloc sec rel =
    if p rel.symbol then (
      (* kind *)
      let kind = match rel.rtype with
	| 0x06 -> 0x0002 (* absolute *)
	| 0x14 -> 0x0001 (* relative *)
	| k    -> 
	    Printf.eprintf "Unsupported relocated kind %04x" k;
	    exit 2
      in
      int32_to_buf data kind;

      (* name *)
      Reloc.abs sect (Int32.of_int (Buffer.length data)) strsym;
      int32_to_buf data (Buffer.length strings);
      Buffer.add_string strings rel.symbol.sym_name;
      Buffer.add_char strings '\000';

      (* target *)
      let target = Symbol.intern sec rel.addr in
      Reloc.abs sect (Int32.of_int (Buffer.length data)) target;
      int32_to_buf data 0;
      syms := target :: !syms;
      false
    ) else true
  in
  let section sec = sec.relocs <- filter (reloc sec) sec.relocs in
  List.iter section x.sections;
  int32_to_buf data 0;

  strsym.value <- Int32.of_int (Buffer.length data);
  sect.data <- Buffer.contents data ^ Buffer.contents strings;
  x.sections <- sect :: x.sections;
  x.symbols <- 
    (Symbol.export "dynreloc" sect 0l) ::
    strsym :: !syms @ List.filter (fun x -> not (p x)) x.symbols


let make_symtable x p =
  let sect = Section.create ".dynsym" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let strsym = Symbol.intern sect 0l in

  let exports = filter p x.symbols in
  int32_to_buf data (List.length exports);
  List.iter
    (fun s  ->
       Reloc.abs sect (Int32.of_int (Buffer.length data)) s;
       int32_to_buf data 0;

       Reloc.abs sect (Int32.of_int (Buffer.length data)) strsym;
       int32_to_buf data (Buffer.length strings);
       Buffer.add_string strings s.sym_name;
       Buffer.add_char strings '\000';
    )
    exports;
  strsym.value <- Int32.of_int (Buffer.length data);
  sect.data <- Buffer.contents data ^ Buffer.contents strings;
  x.sections <- sect :: x.sections;
  x.symbols <- (Symbol.export "dynsytbl" sect 0l) :: strsym :: x.symbols

let has_prefix pr s =
  String.length s > String.length pr && String.sub s 0 (String.length pr) = pr


let parse_obj s = 
  let ic = open_in_bin s in
  let coff = Coff.get ic 0 in
  if Array.length Sys.argv > 2 then
    let oc = open_out_bin Sys.argv.(2) in
    make_symtable coff (fun s -> Symbol.is_export s && has_prefix "_caml" s.sym_name);
    make_reloctable coff (fun s -> Symbol.is_extern s && has_prefix "_caml" s.sym_name);
    Coff.put oc coff
  else
    Coff.dump coff

module StrSet = Set.Make(String)

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

(* TODO: write a real lexer for directives (allow the -opt syntax) *)
let parse_directives s =
  let deflibs = ref [] in
  let rec aux l =
    let i = String.index_from s l '/' in
    if String.uppercase (String.sub s (i+1) 11) = "DEFAULTLIB:" then (
      let j = String.index_from s (i+12) ' ' in
      let lib = String.sub s (i+12) (j-i-12) in
      let lib = 
	if lib.[0] = '"' && lib.[String.length lib - 1]  = '"'
	then String.sub lib 1 (String.length lib - 2)
	else lib in
      deflibs := lib :: !deflibs;
      aux j
    ) else
      aux (succ i)
  in
  (try aux 0 with _ -> ());
  !deflibs
      

let () =
(*  parse_obj Sys.argv.(1);   *)

  let libpath = parse_libpath (try Sys.getenv "LIB" with Not_found -> "") in
  let defined = ref StrSet.empty in
  let files = ref [] in
  let loaded = ref StrSet.empty in

  let find_file fn =
    if Sys.file_exists fn then fn
    else 
      Filename.concat
	(List.find (fun s -> Sys.file_exists (Filename.concat s fn)) libpath)
	fn in

  let find_file fn =
    try find_file fn
    with Not_found ->
      try find_file (fn ^ ".lib")
      with Not_found -> failwith ("Cannot find file " ^ fn) in

  let aliases = Hashtbl.create 16 in

  let rec normalize name =
    try normalize (Hashtbl.find aliases name)
    with Not_found -> name in

  let needed accu obj =
    List.fold_left
      (fun accu sym -> 
	 if Symbol.is_extern sym
	 then StrSet.add (normalize sym.sym_name) accu
	 else accu
      )
      accu
      obj.symbols
  in

  let rec collect_exports_obj obj =
    List.iter (fun (x,y) -> Hashtbl.add aliases x y) (Coff.aliases obj);
    (try
       let dirsec = 
	 List.find (fun s -> s.sec_name = ".drectve") obj.sections in
       let deflibs = parse_directives dirsec.data in
(*       List.iter (fun s -> Printf.printf "DEFLIB: %s\n" s) deflibs; *)
       List.iter (add_file true) deflibs
     with Not_found -> ());

    List.iter
      (fun sym -> 
	 if Symbol.is_defin sym 
	 then defined := StrSet.add sym.sym_name !defined
      )
      obj.symbols 

  and collect_exports = function
    | `Obj obj -> collect_exports_obj obj
    | `Lib (objs,imports) ->
	List.iter (fun (_,obj) -> collect_exports_obj obj) objs;
	List.iter
	  (fun (s,_) -> 
(*	     Printf.printf "I:%s\n" s; *)
	     defined := StrSet.add s (StrSet.add ("__imp_" ^ s) !defined))
	  imports

  and add_file default fn =
    let fn = find_file fn in
    if StrSet.mem fn !loaded then ()
    else (loaded := StrSet.add fn !loaded;
	  let x = Lib.read fn in
	  files := (default, fn, x) :: !files;
	  collect_exports x)
  in

  for i = 1 to Array.length Sys.argv - 1 do add_file false Sys.argv.(i) done;
  let files = !files in

  List.iter 
    (fun (default,fn,x) ->
       if not default then begin
	 let needed = match x with
	   | `Obj obj -> needed StrSet.empty obj
	   | `Lib (objs,_) -> 
	     List.fold_left (fun accu (_,obj) -> needed accu obj)
	       StrSet.empty objs in
	 let relocs = StrSet.diff needed !defined in
	 StrSet.iter
	   (fun s -> Printf.printf "%s: %s\n" fn s)
	   relocs
       end
    )
    files

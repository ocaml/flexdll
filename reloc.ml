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
  mutable extra_info: [ `Alias of symbol | `Section of section | `None ];
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

  let empty () = 
    { sym_pos = (-1); sym_name = gen_sym(); value = 0l; 
      section = `Num 0; storage = 0; stype = 0; auxn = 0; auxs = "";
      extra_info = `None }

  let intern sec addr =
    { (empty ()) with section = `Section sec; storage = 3; value = addr }

  let export name sec addr = 
    { (empty ()) with sym_name = name; value = addr;
	section = `Section sec; storage = 2 }

  let extern name = 
    { (empty ()) with sym_pos = (-1); sym_name = name; 
	section = `Num 0; storage = 2 }


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
      extra_info = `None;
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
      | { storage = 3; value = 0l } ->
	  Printf.printf "section %s, num %i, select %i\n" sect
	    (int16 s.auxs 12)
	    (int8 s.auxs 14)
      | { storage = 3 } ->
	  Printf.printf "static %s @ 0x%08lx\n" sect s.value
      | { storage = 103 } ->
	  Printf.printf "filename %s\n" (strz s.auxs 0 '\000')
      | { storage = 105 } ->
	  Printf.printf "weak ext\n"
      | _ ->
	  Printf.printf 
	    "value=0x%08lx, sect=%s, storage=%s, aux=%S\n"
	    s.value sect storage s.auxs

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
    match s with
      | { storage = 105; extra_info = `Alias s' } when s'.sym_pos >= 0 ->
	  (* weak ext *)
	  emit_int32 oc (Int32.of_int s'.sym_pos);
	  output_string oc (String.sub s.auxs 4 (String.length s.auxs - 4))
      | { storage = 3; value = 0l; extra_info = `Section s' } ->
	  (* section def *)
	  assert false
      | _ ->
	  if s.storage = 105 then assert (int16 s.auxs 12 = 0);
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
  let empty () =
    { machine = 0x14c; date = 0x4603de0el; 
      sections = []; symbols = []; opts = 0 }

  let parse_directives s =
    let l = String.length s in
    let rec aux0 i = if i = l then [] else match s.[i] with
      | ' ' -> aux0 (i+1)
      | '-' | '/' -> aux1 (i+1) (i+1)
      | _ -> raise Exit
    and aux1 i0 i = if i = l then (String.sub s i0 (i - i0), [])::[] 
    else match s.[i] with
      | 'a'..'z' | 'A'..'Z' -> aux1 i0 (i+1)
      | ' ' -> (String.sub s i0 (i - i0), []) :: aux0 (i+1)
      | ':' -> aux2 (String.sub s i0 (i - i0)) [] (i+1)
      | _   -> raise Exit
    and aux2 cmd args i = match s.[i] with
      | '"' -> aux3 cmd args (i+1) (i+1)
      | _   -> aux4 cmd args i i
    and aux3 cmd args i0 i = match s.[i] with
      | '"' -> aux5 cmd (String.sub s i0 (i - i0) :: args) (i+1)
      | _   -> aux3 cmd args i0 (i+1)
    and aux4 cmd args i0 i = 
      if i = l then (cmd, String.sub s i0 (i - i0) :: args)::[] 
      else match s.[i] with
	| ' ' -> (cmd, String.sub s i0 (i - i0) :: args) :: aux0 (i+1)
	| ',' -> aux2 cmd (String.sub s i0 (i - i0) :: args) (i+1)
	| _   -> aux4 cmd args i0 (i+1)
    and aux5 cmd args i = match s.[i] with
      | ' ' -> (cmd,args) :: aux0 (i+1)
      | ',' -> aux2 cmd args (i+1)
      | _   -> raise Exit
    in
    try List.map (fun (cmd,args) -> (cmd,List.rev args)) (aux0 0)
    with _ ->
      Printf.eprintf "Cannot parse directive: %s\n" s;
      exit 2
 
  let directives obj =
    try 
      parse_directives 
	((List.find (fun s -> s.sec_name = ".drectve") obj.sections).data)
    with Not_found -> []

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
	if i = symcount then List.rev accu
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
	 (match s with
	    | { storage = 105; auxn = 1 } ->
		(* weak ext *)
		(match symtbl.(Int32.to_int (int32 s.auxs 0)) with
		   | Some s -> s.extra_info <- `Alias s
		   | None -> assert false)
	    | { storage = 3; value = 0l; auxn = 1 } ->
		(* section def *)
		if int8 s.auxs 14 = 5 then
		  let num = int16 s.auxs 12 in
		  assert (num > 0);
		  s.extra_info <- `Section sections.(num - 1)
		else
		  assert (int16 s.auxs 12 = 0)
	    | { storage = 103 }
	    | { auxn = 0 } -> ()
	    | _ ->
		Symbol.dump s;
		assert false);
	 (match s.section with
	    | `Num i when i > 0 && i <= Array.length sections ->
		assert (i <= Array.length sections);
		s.section <- `Section sections.(i - 1)
	    | _ -> ()))
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
	 match s.extra_info with 
	   | `Alias s' -> a := (s.sym_name,s'.sym_name) :: !a
	   | _ -> ()
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

let drop_underscore s =
  assert(s.[0] = '_');
  String.sub s 1 (String.length s - 1)

let add_reloctable x p sname =
  let sect = Section.create ".dynrel" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let strsym = Symbol.intern sect 0l in
  let str_pos = Hashtbl.create 16 in

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
    (Symbol.export sname sect 0l) ::
    strsym :: !syms @ List.filter (fun x -> not (p x)) x.symbols


let add_symtable obj exports symname =
  let sect = Section.create ".dynsym" 0xc0300040l in
  let data = Buffer.create 1024 in
  let strings = Buffer.create 1024 in
  let strsym = Symbol.intern sect 0l in
  obj.symbols <- strsym :: (Symbol.export symname sect 0l) :: obj.symbols;
  let exports = List.sort Pervasives.compare exports in
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

let add_master_reloctable obj names symname =
  let sect = Section.create ".dynrel" 0xc0300040l in
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


let has_prefix pr s =
  String.length s > String.length pr && String.sub s 0 (String.length pr) = pr

(*
let parse_obj s = 
  let ic = open_in_bin s in
  let coff = Coff.get ic 0 in
  if Array.length Sys.argv > 2 then
    let oc = open_out_bin Sys.argv.(2) in
    make_symtable coff (fun s -> Symbol.is_export s && has_prefix "_caml" s.sym_name);
    add_reloctable coff (fun s -> Symbol.is_extern s && has_prefix "_caml" s.sym_name) "reloctbl";
    Coff.put oc coff
  else
    Coff.dump coff
*)

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

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

let libpath = parse_libpath (try Sys.getenv "LIB" with Not_found -> "")
  
let find_file fn =
  if Sys.file_exists fn then fn
  else 
    Filename.concat
      (List.find (fun s -> Sys.file_exists (Filename.concat s fn)) libpath)
      fn

let find_file fn =
  (* todo: cache this function *)
  try find_file fn
  with Not_found ->
    try find_file (fn ^ ".lib")
    with Not_found -> failwith (Printf.sprintf "Cannot find file %S" fn)

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

(*
let build_exe files =
  let exports =
    List.fold_left
      (fun accu fn ->
	 let fn = find_file fn in
	 match Lib.read fn with
	   | `Obj obj -> exports accu obj
	   | `Lib (objs,_) ->
	       List.fold_left (fun accu (_,obj) -> exports accu obj) accu objs
      )
      StrSet.empty
      files
  in
  StrSet.iter (fun s -> Printf.printf "-> %s\n" s) exports;
  create_symtable "symtable.obj" (StrSet.elements exports)
*)

let collect f l =
  List.fold_left
    (fun accu x ->
       match f x with None -> accu | Some y -> y :: accu)
    []
    l

let build_dll link_exe output_file files =
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
  
  let needed obj = needed normalize StrSet.empty obj in
  let imports obj = StrSet.diff (needed obj) !defined in
  
  (* Second step: transitive closure, starting from given objects *)

  let libobjects  = Hashtbl.create 16 in
  let objcount = ref 0 in
  let to_link = ref [] in
  let reloctbls = ref [] in
  let exported = ref StrSet.empty in

  let record_obj obj =
    incr objcount;
    let fn = Printf.sprintf "tmpobj%i.obj" !objcount in
    to_link := fn :: !to_link;
    let oc = open_out_bin fn in
    Coff.put oc obj;
    close_out oc in

  let close_obj imps obj = 
    if link_exe then (
      Printf.eprintf "Cannot resolve symbols:\n";
      StrSet.iter
	(fun s -> Printf.eprintf " %s\n" s)
	imps;
      exit 1
    );
    let reloctbl = Symbol.gen_sym () in
    reloctbls := reloctbl :: !reloctbls;
    add_reloctable obj (fun s -> StrSet.mem s.sym_name imps) reloctbl;
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
       let imps = imports obj in
       if (StrSet.is_empty imps) then to_link := fn :: !to_link
       else close_obj imps obj;
       link_obj obj
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
       if Hashtbl.mem to_explode libname then close_obj imps obj
       else libs := StrSet.add libname !libs 
    )
    libobjects;
  StrSet.iter (fun l -> 
		 if not (Hashtbl.mem to_explode l) 
		 then to_link := l :: !to_link) 
    !libs;
  let obj = Coff.empty () in
  add_symtable obj (StrSet.elements !exported) 
    (if link_exe then "_static_symtable" else "_symtbl");
  if not link_exe then add_master_reloctable obj !reloctbls "_reloctbl";
  record_obj obj;

  let cmd = 
    if link_exe
    then
      Printf.sprintf 
	"link /out:%s %s"
	output_file
	(String.concat " " (!to_link @ List.map (fun (fn,_,_) -> fn) ilibs))
    else
      Printf.sprintf 
	"link /dll /defaultlib:msvcrt /export:symtbl /export:reloctbl /out:%s %s"
	output_file
	(String.concat " " (!to_link @ List.map (fun (fn,_,_) -> fn) ilibs))
  in
  Printf.printf "+ %s\n" cmd;
  ignore (Sys.command cmd)


let files = ref []
let output_file = ref ""
let exe_mode = ref false  
let usage = "reloc -o <result.dll> file1.obj file2.obj ..."
let specs = [
  "-o", Arg.Set_string output_file, 
  " choose the name of the output file";

  "-exe", Arg.Set exe_mode,
  " link an executable (not a dll)";
]

let () =
  Arg.parse (Arg.align specs) (fun x -> files := x :: !files) usage;
  if !output_file = "" then 
    if !exe_mode then output_file := "result.exe"
    else output_file := "result.dll";
  build_dll !exe_mode !output_file !files

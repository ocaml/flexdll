demo: dump b.dll c.dll
	./dump b.dll c.dll

reloc: reloc.ml
	ocamlopt -o reloc reloc.ml

dump: dump.c
	cl /Fe"dump.exe" /MD dump.c

a2.obj: a.ml reloc
	ocamlopt -c a.ml
	./reloc a.obj a2.obj

b2.obj: b.c reloc
	cl /c /MD b.c
	./reloc b.obj b2.obj

c2.obj: c.c reloc
	cl /c /MD c.c
	./reloc c.obj c2.obj


a.dll: a2.obj
	link /dll /out:a.dll a2.obj /DEFAULTLIB:"MSVCRT" /export:dynreloc /export:dynsytbl

b.dll: b2.obj
	link /dll /out:b.dll b2.obj /DEFAULTLIB:"MSVCRT" /export:dynreloc /export:dynsytbl

c.dll: c2.obj
	link /dll /out:c.dll c2.obj /DEFAULTLIB:"MSVCRT" /export:dynreloc /export:dynsytbl

clean:
	rm -f *.obj *.exe *.cmx *.dll *.manifest *.exp *.lib *.cmi reloc reloc.exe *.o *~
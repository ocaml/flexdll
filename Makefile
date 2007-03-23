CC = cl /nologo /MD

demo: dump b.dll c.dll
	./dump b.dll c.dll

reloc: reloc.ml
	ocamlopt -o reloc reloc.ml

dump: dynsyms.obj dump.obj
	$(CC) /Fe"dump.exe" dynsyms.obj dump.obj

dynsyms.obj: dynsyms.h dynsyms.c
	$(CC) /c dynsyms.c

dump.obj: dynsyms.h dump.c
	$(CC) /c dump.c

a2.obj: a.ml reloc
	ocamlopt -c a.ml
	./reloc a.obj a2.obj

b.obj: b.c
	$(CC) /c b.c

c.obj: c.c
	$(CC) /c c.c

z.dll: b.obj c.obj reloc
	./reloc b.obj c.obj
	link /dll /out:result.dll tmpobj3.obj tmpobj2.obj tmpobj1.obj


b2.obj: b.obj reloc
	./reloc b.obj b2.obj

c2.obj: c.obj reloc
	./reloc c.obj c2.obj


a.dll: a2.obj
	link /dll /out:a.dll a2.obj /DEFAULTLIB:"MSVCRT" /export:dynreloc /export:dynsytbl

b.dll: b2.obj
	link /dll /out:b.dll b2.obj /DEFAULTLIB:"MSVCRT" /export:dynreloc /export:dynsytbl

c.dll: c2.obj
	link /dll /out:c.dll c2.obj /DEFAULTLIB:"MSVCRT" /export:dynreloc /export:dynsytbl

clean:
	rm -f *.obj *.exe *.cmx *.dll *.manifest *.exp *.lib *.cmi reloc reloc.exe *.o *~
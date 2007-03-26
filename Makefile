CC = cl /nologo /MD

demo: dump b.dll c.dll
	./dump b.dll c.dll

reloc: reloc.ml
	ocamlopt -o reloc reloc.ml

dump: dynsyms.obj dump.obj reloc
	./reloc -exe -o dump.exe dynsyms.obj dump.obj

dynsyms.obj: dynsyms.h dynsyms.c
	$(CC) /c dynsyms.c

dump.obj: dynsyms.h dump.c
	$(CC) /c dump.c

b.obj: b.c
	$(CC) /c b.c

c.obj: c.c
	$(CC) /c c.c

b.dll: b.obj reloc
	./reloc -o b.dll b.obj

c.dll: c.obj reloc
	./reloc -o c.dll c.obj

bc.dll: b.obj c.obj reloc
	./reloc -o bc.dll b.obj c.obj


clean:
	rm -f *.obj *.exe *.cmx *.dll *.manifest *.exp *.lib *.cmi reloc reloc.exe *.o *~
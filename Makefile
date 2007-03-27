CC = cl /nologo /MD
O = obj
CHAIN = msvc

#CC = gcc -D_CYGWIN_
#O = o
#CHAIN=cygwin

#CC = gcc -D_MINGW_ -mno-cygwin
#O = o
#CHAIN = mingw

RELOC=./reloc.exe -chain $(CHAIN)

.PHONY: dump
dump: dump.exe

.PHONY: reloc
reloc: reloc.exe

demo: dump.exe b.dll c.dll
	./dump.exe b.dll c.dll

reloc.exe: reloc.ml coff.ml
	ocamlopt -o reloc.exe coff.ml reloc.ml

dump.exe: dynsyms.$(O) dump.$(O) reloc.exe
	$(RELOC) -exe -o dump.exe dynsyms.$(O) dump.$(O)

dynsyms.$(O): dynsyms.h dynsyms.c
	$(CC) -c dynsyms.c

dump.$(O): dynsyms.h dump.c
	$(CC) -c dump.c

b.$(O): b.c
	$(CC) -c b.c

c.$(O): c.c
	$(CC) -c c.c

b.dll: b.$(O) reloc.exe
	$(RELOC) -o b.dll b.$(O)

c.dll: c.$(O) reloc.exe
	$(RELOC) -o c.dll c.$(O)

bc.dll: b.$(O) c.$(O) reloc.exe
	$(RELOC) -o bc.dll b.$(O) c.$(O)


clean:
	rm -f *.obj *.o *.lib *.a *.exe *.cmx *.dll *.manifest *.exp *.cmi reloc reloc.exe *~
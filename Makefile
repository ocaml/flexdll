# Compilers

MSVCC = cl /nologo /MD
CYGCC = gcc -D_CYGWIN_ 
MINCC = gcc -D_MINGW_ -mno-cygwin
OCAMLOPT = ocamlopt

#CC = cl /nologo /MD 
#O = obj
#CHAIN = msvc

#CC = gcc -D_CYGWIN_
#O = o
#CHAIN=cygwin

#CC = gcc -D_MINGW_ -mno-cygwin
#O = o
#CHAIN = mingw

RELOC=./flexlink.exe -chain $(CHAIN)

all: flexlink.exe flexdll_win32.obj flexdll_cygwin.o flexdll_mingw.o

flexlink.exe: reloc.ml coff.ml
	$(OCAMLOPT) -o flexlink.exe coff.ml reloc.ml

flexdll_win32.obj: flexdll.h flexdll.c
	$(MSVCC) -c flexdll.c

flexdll_cygwin.o: flexdll.h flexdll.c
	$(CYGCC) -c flexdll.c

flexdll_mingw.o: flexdll.h flexdll.c
	$(MINCC) -c flexdll.c

demo: dump.exe b.dll c.dll
	./dump.exe b.dll c.dll

dump.exe: flexdll.$(O) dump.$(O) flexlink.exe
	$(RELOC) -exe -o dump.exe flexdll.$(O) dump.$(O)

flexdll.$(O): flexdll.h flexdll.c
	$(CC) -c flexdll.c

dump.$(O): flexdll.h dump.c
	$(CC) -c dump.c

b.$(O): b.c
	$(CC) -c b.c

c.$(O): c.c
	$(CC) -c c.c

b.dll: b.$(O) flexlink.exe
	$(RELOC) -o b.dll b.$(O)

c.dll: c.$(O) flexlink.exe
	$(RELOC) -o c.dll c.$(O)

bc.dll: b.$(O) c.$(O) flexlink.exe
	$(RELOC) -o bc.dll b.$(O) c.$(O)


clean:
	rm -f *.obj *.o *.lib *.a *.exe *.cmx *.dll *.manifest *.exp *.cmi *~
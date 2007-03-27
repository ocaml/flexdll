# Compilers

MSVCC = cl /nologo /MD
CYGCC = gcc -D_CYGWIN_ 
MINCC = gcc -D_MINGW_ -mno-cygwin
OCAMLOPT = ocamlopt

all: flexlink.exe flexdll_msvc.obj flexdll_cygwin.o flexdll_mingw.o

flexlink.exe: reloc.ml coff.ml
	$(OCAMLOPT) -o flexlink.exe coff.ml reloc.ml

flexdll_msvc.obj: flexdll.h flexdll.c
	$(MSVCC) -c /Fo"flexdll_msvc.obj" flexdll.c

flexdll_cygwin.o: flexdll.h flexdll.c
	$(CYGCC) -c -o flexdll_cygwin.o flexdll.c

flexdll_mingw.o: flexdll.h flexdll.c
	$(MINCC) -c -o flexdll_mingw.o flexdll.c

demo_msvc: flexlink.exe flexdll_msvc.obj
	(cd test && $(MAKE) clean demo CHAIN=msvc CC="$(MSVCC)" O=obj)

demo_cygwin: flexlink.exe flexdll_cygwin.o
	(cd test && $(MAKE) clean demo CHAIN=cygwin CC="$(CYGCC)" O=o)

demo_mingw: flexlink.exe flexdll_mingw.o
	(cd test && $(MAKE) clean demo CHAIN=mingw CC="$(MINCC)" O=o)

clean:
	rm -f *.obj *.o *.lib *.a *.exe *.cmx *.dll *.manifest *.exp *.cmi *~
	cd test && $(MAKE) clean

PACKAGE = flexdll-alpha-`date +%Y%m%d`.tar.gz

package:
	rm -Rf flexdll
	mkdir flexdll
	mkdir flexdll/test
	cp -a *.c *.ml *.h Makefile LICENSE README flexdll/
	cp -aR test/Makefile test/*.c flexdll/test/
	tar czf $(PACKAGE) flexdll
	rm -Rf flexdll

upload:
	scp $(PACKAGE) frisch.fr:www/flexdll/
VERSION = 0.10
all: flexlink.exe support

.PHONY: version.ml
version.ml:
	echo "let version = \"$(VERSION)\"" > version.ml

# Supported tool-chains

CHAINS = mingw cygwin msvc


# Compilers

MSVCC = cl /nologo /MD -D_CRT_SECURE_NO_DEPRECATE
CYGCC = gcc 
MINCC = gcc -mno-cygwin
OCAMLOPT = ocamlopt
#OCAMLOPT = FLEXLINKFLAGS=-real-manifest ocamlopt
#LINKFLAGS = unix.cmxa
LINKFLAGS =

support:
	for i in $(CHAINS); do $(MAKE) build_$$i ; done 

build_msvc: flexdll_msvc.obj flexdll_initer_msvc.obj
build_cygwin: flexdll_cygwin.o flexdll_initer_cygwin.o 
build_mingw: flexdll_mingw.o flexdll_initer_mingw.o 

OBJS = version.ml coff.ml cmdline.ml reloc.ml

flexlink.exe: $(OBJS)
	@echo Building flexlink.exe with TOOLCHAIN=$(TOOLCHAIN)
	rm -f flexlink.exe
	$(OCAMLOPT) -o flexlink.exe $(LINKFLAGS) $(OBJS)

flexdll_msvc.obj: flexdll.h flexdll.c
	$(MSVCC) -c /Fo"flexdll_msvc.obj" flexdll.c

flexdll_cygwin.o: flexdll.h flexdll.c
	$(CYGCC) -c -o flexdll_cygwin.o flexdll.c

flexdll_mingw.o: flexdll.h flexdll.c
	$(MINCC) -c -o flexdll_mingw.o flexdll.c

flexdll_initer_mingw.o: flexdll_initer.c
	$(MINCC) -c -o flexdll_initer_mingw.o flexdll_initer.c

flexdll_initer_cygwin.o: flexdll_initer.c
	$(CYGCC) -c -o flexdll_initer_cygwin.o flexdll_initer.c

flexdll_initer_msvc.obj: flexdll_initer.c
	$(MSVCC) -c /Fo"flexdll_initer_msvc.obj" flexdll_initer.c

demo_msvc: flexlink.exe flexdll_msvc.obj
	(cd test && $(MAKE) clean demo CHAIN=msvc CC="$(MSVCC)" O=obj)

demo_cygwin: flexlink.exe flexdll_cygwin.o
	(cd test && $(MAKE) clean demo CHAIN=cygwin CC="$(CYGCC)" O=o)

demo_mingw: flexlink.exe flexdll_mingw.o
	(cd test && $(MAKE) clean demo CHAIN=mingw CC="$(MINCC)" O=o)

demo_msvc64:  flexlink.exe flexdll_msvc.obj
	(cd test && $(MAKE) clean demo CHAIN=msvc CC="$(MSVCC)" O=obj EXTRA_OPTS="-x64 bufferoverflowu.lib")

clean:
	rm -f *.obj *.o *.lib *.a *.exe *.cmx *.dll *.exp *.cmi *~
	cd test && $(MAKE) clean


## Packaging

COMMON_FILES = LICENSE README CHANGES flexdll.h flexdll.c flexdll_initer.c default.manifest
URL = frisch@frisch.fr:www/flexdll/

# Source packages

PACKAGE = flexdll-$(VERSION).tar.gz

package_src:
	rm -Rf flexdll
	mkdir flexdll
	mkdir flexdll/test
	cp -a *.ml Makefile $(COMMON_FILES) flexdll/
	cp -aR test/Makefile test/*.c flexdll/test/
	tar czf $(PACKAGE) flexdll
	rm -Rf flexdll

upload:
	rsync $(PACKAGE) CHANGES $(URL)

upload_dev:
	$(MAKE) VERSION=dev upload_src

upload_src: package_src upload

# Binary package

PACKAGE_BIN = flexdll-bin-$(VERSION)$(PACKAGE_BIN_SUFFIX).zip

package_bin:
	$(MAKE) clean all
	rm -f $(PACKAGE_BIN)
	zip $(PACKAGE_BIN) $(COMMON_FILES) \
	    flexlink.exe flexdll_*.obj flexdll_*.o

do_upload_bin:
	rsync $(PACKAGE_BIN) $(URL)

upload_bin: package_bin do_upload_bin

upload_bin_64:
	PACKAGE_BIN_SUFFIX=-amd64 $(MAKE) upload_bin

include $(shell cygpath -ad "$(shell ocamlopt -where)/Makefile.config")

show_toolchain:
	@echo Toolchain for the visible ocamlopt: $(TOOLCHAIN)


swap:
	NOMLFICORE=1 $(OCAMLOPT) -o flexlink-new.exe $(LINKFLAGS) $(OBJS)
	cp flexlink.exe flexlink.exe.bak
	cp flexlink-new.exe flexlink.exe

PREFIX = "C:\Program Files\flexdll"

install:
	mkdir -p $(PREFIX)
	cp $(COMMON_FILES) flexlink.exe flexdll_*.obj flexdll_*.o $(PREFIX)

VERSION = 0.24
all: flexlink.exe support

include $(shell cygpath -ad "$(shell ocamlopt -where)/Makefile.config")

.PHONY: version.ml
version.ml:
	echo "let version = \"$(VERSION)\"" > version.ml

# Supported tool-chains

CHAINS = mingw cygwin msvc msvc64


# Compilers

# This Makefile assumes the 32-bit version of VS 2008 or Win7 SDK is in the path.

MSVCC_ROOT = $(shell which cl.exe | cygpath -f - -ad | xargs -d \\n dirname | cygpath -f - -m)
MSVC_LIB1 = $(shell dirname $(MSVCC_ROOT))
MSVC_LIB2 = $(shell which ResGen.exe | cygpath -f - -ad | xargs -d \\n dirname | xargs -d \\n dirname | cygpath -f - -m)
MSVC_LIB = $(MSVC_LIB1)/Lib;$(MSVC_LIB2)/Lib
MSVC_INCLUDE = $(MSVC_LIB1)/Include;$(MSVC_LIB2)/Include
MSVC_PREFIX=LIB="$(MSVC_LIB)" INCLUDE="$(MSVC_INCLUDE)" 

MSVC64_LIB = $(MSVC_LIB1)/Lib/amd64;$(MSVC_LIB2)/Lib/x64
MSVC64_PREFIX=LIB="$(MSVC64_LIB)" INCLUDE="$(MSVC_INCLUDE)" 

show_root:
	@echo "$(MSVCC_ROOT)"
	@echo "$(MSVC_LIB)"

MSVCC = $(MSVCC_ROOT)/cl.exe /nologo /MD -D_CRT_SECURE_NO_DEPRECATE /GS-
MSVCC64 = $(MSVCC_ROOT)/amd64/cl.exe /nologo /MD -D_CRT_SECURE_NO_DEPRECATE /GS-
CYGCC = gcc 
MINCC = gcc -mno-cygwin
OCAMLOPT = ocamlopt
#OCAMLOPT = FLEXLINKFLAGS=-real-manifest ocamlopt
#LINKFLAGS = unix.cmxa

#ifeq ($(SYSTEM), win64)
#LINKFLAGS=
#else
LINKFLAGS = -ccopt "-link version_res.o"
#endif

support:
	for i in $(CHAINS); do $(MAKE) build_$$i ; done 

build_msvc: flexdll_msvc.obj flexdll_initer_msvc.obj
build_msvc64: flexdll_msvc64.obj flexdll_initer_msvc64.obj
build_cygwin: flexdll_cygwin.o flexdll_initer_cygwin.o 
build_mingw: flexdll_mingw.o flexdll_initer_mingw.o 

OBJS = version.ml coff.ml cmdline.ml create_dll.ml reloc.ml

flexlink.exe: $(OBJS)
	@echo Building flexlink.exe with TOOLCHAIN=$(TOOLCHAIN)
	rm -f flexlink.exe
	windres version.rc version_res.o
	$(OCAMLOPT) -o flexlink.exe $(LINKFLAGS) $(OBJS)

flexdll_msvc.obj: flexdll.h flexdll.c
	$(MSVC_PREFIX) $(MSVCC) -c /Fo"flexdll_msvc.obj" flexdll.c

flexdll_msvc64.obj: flexdll.h flexdll.c
	$(MSVC64_PREFIX) $(MSVCC64) -c /Fo"flexdll_msvc64.obj" flexdll.c

flexdll_cygwin.o: flexdll.h flexdll.c
	$(CYGCC) -c -o flexdll_cygwin.o flexdll.c

flexdll_mingw.o: flexdll.h flexdll.c
	$(MINCC) -c -o flexdll_mingw.o flexdll.c

flexdll_initer_msvc.obj: flexdll_initer.c
	$(MSVC_PREFIX) $(MSVCC) -c /Fo"flexdll_initer_msvc.obj" flexdll_initer.c

flexdll_initer_msvc64.obj: flexdll_initer.c
	$(MSVC64_PREFIX) $(MSVCC64) -c /Fo"flexdll_initer_msvc64.obj" flexdll_initer.c

flexdll_initer_cygwin.o: flexdll_initer.c
	$(CYGCC) -c -o flexdll_initer_cygwin.o flexdll_initer.c

flexdll_initer_mingw.o: flexdll_initer.c
	$(MINCC) -c -o flexdll_initer_mingw.o flexdll_initer.c

demo_msvc: flexlink.exe flexdll_msvc.obj flexdll_initer_msvc.obj
	(cd test && $(MSVC_PREFIX) $(MAKE) clean demo CHAIN=msvc CC="$(MSVCC)" O=obj)

demo_cygwin: flexlink.exe flexdll_cygwin.o flexdll_initer_cygwin.o
	(cd test && $(MAKE) clean demo CHAIN=cygwin CC="$(CYGCC)" O=o)

demo_mingw: flexlink.exe flexdll_mingw.o flexdll_initer_mingw.o
	(cd test && $(MAKE) clean demo CHAIN=mingw CC="$(MINCC)" O=o)

#demo_msvc64:  flexlink.exe flexdll_msvc.obj flexdll_initer_msvc.obj
#	(cd test && $(MAKE) clean demo CHAIN=msvc CC="$(MSVCC)" O=obj EXTRA_OPTS="-x64 bufferoverflowu.lib")

demo_msvc64:  flexlink.exe flexdll_msvc64.obj flexdll_initer_msvc64.obj
	(cd test && $(MSVC64_PREFIX) $(MAKE) clean demo CHAIN=msvc64 CC="$(MSVCC64)" O=obj)
#demo_msvc64:
#	(cd test && $(MAKE) clean demo CHAIN=msvc64 CC="$(MSVCC)" O=obj)

clean:
	rm -f *.obj *.o *.lib *.a *.exe *.cmx *.dll *.exp *.cmi *~
	cd test && $(MAKE) clean


## Packaging

COMMON_FILES = LICENSE README CHANGES flexdll.h flexdll.c flexdll_initer.c default.manifest default_amd64.manifest
URL = frisch@frisch.fr:www/flexdll/

# Source packages

PACKAGE = flexdll-$(VERSION).tar.gz

package_src:
	rm -Rf flexdll
	mkdir flexdll
	mkdir flexdll/test
	cp -a *.ml Makefile $(COMMON_FILES) version.rc flexdll/
	cp -aR test/Makefile test/*.c flexdll/test/
	tar czf $(PACKAGE) flexdll
	rm -Rf flexdll

upload:
	rsync $(PACKAGE) CHANGES LICENSE $(URL)

upload_dev:
	$(MAKE) VERSION=dev upload_src

upload_src: package_src upload

# Binary package

PACKAGE_BIN = flexdll-bin-$(VERSION)$(PACKAGE_BIN_SUFFIX).zip
INSTALLER = flexdll-$(VERSION)$(PACKAGE_BIN_SUFFIX)-setup.exe

package_bin:
	$(MAKE) clean all
	rm -f $(PACKAGE_BIN)
	zip $(PACKAGE_BIN) $(COMMON_FILES) \
	    flexlink.exe flexdll_*.obj flexdll_*.o

do_upload_bin:
	rsync $(PACKAGE_BIN) $(URL)

upload_bin: package_bin do_upload_bin

show_toolchain:
	@echo Toolchain for the visible ocamlopt: $(TOOLCHAIN)

swap:
	NOMLFICORE=1 $(OCAMLOPT) -o flexlink-new.exe $(LINKFLAGS) $(OBJS)
	cp flexlink.exe flexlink.exe.bak
	cp flexlink-new.exe flexlink.exe

#PREFIX = "C:\Program Files\flexdll"
#
#install:
#	mkdir -p $(PREFIX)
#	cp $(COMMON_FILES) flexlink.exe flexdll_*.obj flexdll_*.o $(PREFIX)

installer:
	rm -rf flexdll_install_files
	mkdir flexdll_install_files
	(cd flexdll_install_files && unzip ../$(PACKAGE_BIN))
	/cygdrive/c/Program\ Files/NSIS/makensis installer.nsi
	mv flexdll_setup.exe $(INSTALLER)

upload_installer:
	rsync $(INSTALLER) $(URL)


upload_all:
	$(MAKE) upload_src upload_bin installer upload_installer

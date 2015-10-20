VERSION = 0.34
all: flexlink.exe support

include $(shell cygpath -ad "$(shell ocamlopt -where)/Makefile.config")

MINGW_PREFIX = i686-w64-mingw32
MINCC = $(MINGW_PREFIX)-gcc

MINGW64_PREFIX = x86_64-w64-mingw32
MIN64CC = $(MINGW64_PREFIX)-gcc

CYGWIN_PREFIX = i686-pc-cygwin
CYGCC = $(CYGWIN_PREFIX)-gcc

CYGWIN64_PREFIX = x86_64-pc-cygwin
CYG64CC = $(CYGWIN64_PREFIX)-gcc

.PHONY: version.ml
version.ml:
	echo "let version = \"$(VERSION)\"" > version.ml
	echo "let mingw_prefix = \"$(MINGW_PREFIX)\"" >> version.ml
	echo "let mingw64_prefix = \"$(MINGW64_PREFIX)\"" >> version.ml

# Supported tool-chains

CHAINS = mingw mingw64 cygwin cygwin64 msvc msvc64

# Compilers

# Attempt to locate the Visual Studio/Windows SDK

VSCONTOOLS = $(lastword $(foreach ver,8 9 10 11 12 13 14 15 16 17 18 19 20,\
	$(if $(VS$(ver)0COMNTOOLS), VS$(ver)0COMNTOOLS)))

VS_HELPER = vs.bat
$(VS_HELPER):
	@echo @call \"%$(VSCONTOOLS)%..\\..\\VC\\vcvarsall.bat\" %1 > $@
	@echo shift >> $@
	@echo set cmd=%1 %2 %3 %4 %5 %6 %7 %8 %9 >> $@
	@for i in {1..9}; do echo shift >> $@; done 
	@echo set cmd=%cmd% %1 %2 %3 %4 %5 %6 %7 %8 %9 >> $@
	@echo %cmd% >> $@

MSVC_PREFIX=./vs.bat x86
MSVC64_PREFIX=./vs.bat x86_amd64
MSVCC=cl.exe /nologo /MD -D_CRT_SECURE_NO_DEPRECATE /GS-
MSVCC64=cl.exe /nologo /MD -D_CRT_SECURE_NO_DEPRECATE /GS-

check_vs: $(VS_HELPER)
	@$(MSVC_PREFIX) cl -? > _output ; rm _output
	@$(MSVC64_PREFIX) cl -? > _output ; rm _output

OCAMLOPT = ocamlopt
#OCAMLOPT = FLEXLINKFLAGS=-real-manifest ocamlopt
#LINKFLAGS = unix.cmxa

ifeq ($(TOOLCHAIN), msvc)
RES=version.res
ifeq ($(ARCH), amd64)
RES_PREFIX=$(MSVC64_PREFIX)
else
RES_PREFIX=$(MSVC_PREFIX)
endif
RES_HELPER=$(VS_HELPER)
else
RES=version_res.o
RES_PREFIX=
RES_HELPER=
endif

ifeq ($(NATDYNLINK), false)
#when ocaml is not built with flexlink i.e. -no-shared-libs
LINKFLAGS = -cclib "$(RES)"
else
LINKFLAGS = -cclib "-link $(RES)"
endif

support:
	for i in $(CHAINS); do $(MAKE) build_$$i; done 

build_gnat: flexdll_gnat.o flexdll_initer_gnat.o
build_msvc: flexdll_msvc.obj flexdll_initer_msvc.obj
build_msvc64: flexdll_msvc64.obj flexdll_initer_msvc64.obj
build_cygwin: flexdll_cygwin.o flexdll_initer_cygwin.o 
build_cygwin64: flexdll_cygwin64.o flexdll_initer_cygwin64.o 
build_mingw: flexdll_mingw.o flexdll_initer_mingw.o 
build_mingw64: flexdll_mingw64.o flexdll_initer_mingw64.o

OBJS = version.ml coff.ml cmdline.ml create_dll.ml reloc.ml

flexlink.exe: $(OBJS) $(RES) $(RES_HELPER)
	@echo Building flexlink.exe with TOOLCHAIN=$(TOOLCHAIN)
	rm -f flexlink.exe
	$(RES_PREFIX) $(OCAMLOPT) -g -w -105 -o flexlink.exe $(LINKFLAGS) $(OBJS)

version.res: version.rc $(RES_HELPER)
	$(RES_PREFIX) rc version.rc

version_res.o: version.rc
	$(TOOLPREF)windres version.rc version_res.o

flexdll_msvc.obj: flexdll.h flexdll.c $(VS_HELPER)
	$(MSVC_PREFIX) $(MSVCC) /DMSVC -c /Fo"flexdll_msvc.obj" flexdll.c

flexdll_msvc64.obj: flexdll.h flexdll.c $(VS_HELPER)
	$(MSVC64_PREFIX) $(MSVCC64) /DMSVC  -c /Fo"flexdll_msvc64.obj" flexdll.c

flexdll_cygwin.o: flexdll.h flexdll.c
	$(CYGCC) -c -DCYGWIN -o flexdll_cygwin.o flexdll.c

flexdll_cygwin64.o: flexdll.h flexdll.c
	$(CYG64CC) -c -DCYGWIN -o flexdll_cygwin64.o flexdll.c

flexdll_mingw.o: flexdll.h flexdll.c
	$(MINCC) -c -DMINGW -o flexdll_mingw.o flexdll.c

flexdll_gnat.o: flexdll.h flexdll.c
	gcc -c -o flexdll_gnat.o flexdll.c

flexdll_mingw64.o: flexdll.h flexdll.c
	$(MIN64CC) -c -DMINGW -o flexdll_mingw64.o flexdll.c

flexdll_initer_msvc.obj: flexdll_initer.c $(VS_HELPER)
	$(MSVC_PREFIX) $(MSVCC) -c /Fo"flexdll_initer_msvc.obj" flexdll_initer.c

flexdll_initer_msvc64.obj: flexdll_initer.c $(VS_HELPER)
	$(MSVC64_PREFIX) $(MSVCC64) -c /Fo"flexdll_initer_msvc64.obj" flexdll_initer.c

flexdll_initer_cygwin.o: flexdll_initer.c
	$(CYGCC) -c -o flexdll_initer_cygwin.o flexdll_initer.c

flexdll_initer_cygwin64.o: flexdll_initer.c
	$(CYG64CC) -c -o flexdll_initer_cygwin64.o flexdll_initer.c

flexdll_initer_mingw.o: flexdll_initer.c
	$(MINCC) -c -o flexdll_initer_mingw.o flexdll_initer.c

flexdll_initer_gnat.o: flexdll_initer.c
	gcc -c -o flexdll_initer_gnat.o flexdll_initer.c

flexdll_initer_mingw64.o: flexdll_initer.c
	$(MIN64CC) -c -o flexdll_initer_mingw64.o flexdll_initer.c


demo_msvc: flexlink.exe flexdll_msvc.obj flexdll_initer_msvc.obj
	(cd test && $(MSVC_PREFIX) $(MAKE) clean demo CHAIN=msvc CC="$(MSVCC)" O=obj)

demo_cygwin: flexlink.exe flexdll_cygwin.o flexdll_initer_cygwin.o
	(cd test && $(MAKE) clean demo CHAIN=cygwin CC="$(CYGCC)" O=o)

demo_cygwin64: flexlink.exe flexdll_cygwin64.o flexdll_initer_cygwin64.o
	(cd test && $(MAKE) clean demo CHAIN=cygwin64 CC="$(CYG64CC)" O=o RUN="PATH=\"/cygdrive/c/cygwin64/bin:$(PATH)\"")

demo_mingw: flexlink.exe flexdll_mingw.o flexdll_initer_mingw.o
	(cd test && $(MAKE) clean demo CHAIN=mingw CC="$(MINCC)" O=o)

demo_mingw64: flexlink.exe flexdll_mingw64.o flexdll_initer_mingw64.o
	(cd test && $(MAKE) clean demo CHAIN=mingw64 CC="$(MIN64CC)" O=o)

demo_msvc64:  flexlink.exe flexdll_msvc64.obj flexdll_initer_msvc64.obj
	(cd test && $(MSVC64_PREFIX) $(MAKE) clean demo CHAIN=msvc64 CC="$(MSVCC64)" O=obj)

distclean: clean
	rm -f $(VS_HELPER)

clean:
	rm -f *.obj *.o *.lib *.a *.exe *.cmx *.dll *.exp *.cmi *~ version.res version.ml
	cd test && $(MAKE) clean


## Packaging

COMMON_FILES = LICENSE README.md CHANGES flexdll.h flexdll.c flexdll_initer.c default.manifest default_amd64.manifest
URL = frisch@frisch.fr:www/flexdll/

# Source packages

PACKAGE = flexdll-$(VERSION).tar.gz

package_src:
	rm -Rf flexdll-$(VERSION)
	mkdir flexdll-$(VERSION)
	mkdir flexdll-$(VERSION)/test
	cp -a *.ml Makefile $(COMMON_FILES) version.rc flexdll-$(VERSION)/
	cp -aR test/Makefile test/*.c flexdll-$(VERSION)/test/
	tar czf $(PACKAGE) flexdll-$(VERSION)
	rm -Rf flexdll-$(VERSION)

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
	/cygdrive/c/Program\ Files\ \(x86\)/NSIS/makensis installer.nsi
	mv flexdll_setup.exe $(INSTALLER)

upload_installer:
	rsync $(INSTALLER) $(URL)


upload_all:
	$(MAKE) upload_src upload_bin installer upload_installer

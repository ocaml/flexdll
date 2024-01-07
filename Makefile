## Tips to build flexdll:
##
##  To use an already configured 64-bit MSVC toolchain:
##     make all MSVC_DETECT=0 CHAINS="mingw mingw64 cygwin64 msvc64"
##


VERSION = 0.43
all: flexlink.exe support

export override OCAML_DETECT_CMD = detect
SHELL_IS_CMD := \
  $(if $(filter %OCAML_DETECT_CMD%, $(shell echo %OCAML_DETECT_CMD%)),,true)
unexport OCAML_DETECT_CMD
undefine DETECT_WINDOWS_SHELL

NULL_DEVICE := $(if $(SHELL_IS_CMD), NUL, /dev/null)

ifeq ($(SHELL_IS_CMD),)
RM_F = rm -f $(1)
TOUCH = touch
else
RM_F = $(if $(wildcard $(1)),del /f $(subst /,\,$(wildcard ($1))))
TOUCH = type NUL >
endif

OCAML_CONFIG_FILE=$(shell cygpath -ad "$(shell ocamlopt -where 2>$(NULL_DEVICE))/Makefile.config" 2>$(NULL_DEVICE))
include $(OCAML_CONFIG_FILE)
OCAMLOPT=ocamlopt
EMPTY=
SPACE=$(EMPTY) $(EMPTY)
OCAML_VERSION:=$(firstword $(subst ~, ,$(subst +, ,$(shell $(OCAMLOPT) -version 2>$(NULL_DEVICE)))))
ifeq ($(OCAML_VERSION),)
OCAML_VERSION:=0
COMPAT_VERSION:=0
else
COMPAT_VERSION:=$(subst ., ,$(OCAML_VERSION))
COMPAT_VERSION:=$(subst $(SPACE),,$(firstword $(COMPAT_VERSION))$(foreach i,$(wordlist 2,$(words $(COMPAT_VERSION)),$(COMPAT_VERSION)),$(if $(filter 0 1 2 3 4 5 6 7 8 9,$(i)),0,)$(i)))
endif

MINGW_PREFIX = i686-w64-mingw32-
MINCC = $(MINGW_PREFIX)gcc

MINGW64_PREFIX = x86_64-w64-mingw32-
MIN64CC = $(MINGW64_PREFIX)gcc

CYGWIN64_PREFIX = x86_64-pc-cygwin-
CYG64CC = $(CYGWIN64_PREFIX)gcc

define NEWLINE


endef
$(SPACE) :=

version.ml: Makefile
	$(file >$@,$\
    let version = "$(VERSION)"$(NEWLINE)$\
    let mingw_prefix = "$(MINGW_PREFIX)"$(NEWLINE)$\
    let mingw64_prefix = "$(MINGW64_PREFIX)")

# Supported tool-chains

CHAINS = mingw mingw64 cygwin64 msvc msvc64

# Compilers

# NB MSVC_DETECT is expected by OCaml's build system
MSVC_DETECT=1

# Attempt to locate the Windows SDK

ifeq ($(MSVC_DETECT),1)
ifeq ($(if $(MAKECMDGOALS),$(if $(strip $(filter-out clean,$(MAKECMDGOALS))),,1)),)
include Makefile.winsdk
endif
endif

Makefile.winsdk: msvs-detect
	bash ./msvs-detect --output=make > $@

MSVC_FLAGS=/nologo /MD -D_CRT_SECURE_NO_DEPRECATE /GS-

ifeq ($(MSVC_DETECT),0)
# Assume that the environment is correctly set for a single Microsoft C Compiler; don't attempt to guess anything
MSVCC_ROOT=
MSVC_PREFIX=
MSVC64_PREFIX=
MSVCC=cl.exe $(MSVC_FLAGS)
MSVCC64=cl.exe $(MSVC_FLAGS)
else
ifeq ($(MSVS_PATH),)
# Otherwise, assume the 32-bit version of VS 2008 or Win7 SDK is in the path.

MSVCC_ROOT := $(shell which cl.exe 2>/dev/null | cygpath -f - -ad | xargs -d \\n dirname 2>/dev/null | cygpath -f - -m)
MSVC_LIB1 = $(shell dirname $(MSVCC_ROOT))
MSVC_LIB2 = $(shell which ResGen.exe | cygpath -f - -ad | xargs -d \\n dirname | xargs -d \\n dirname | cygpath -f - -m)
MSVC_LIB = $(MSVC_LIB1)/Lib;$(MSVC_LIB2)/Lib
MSVC_INCLUDE = $(MSVC_LIB1)/Include;$(MSVC_LIB2)/Include
MSVC_PREFIX=LIB="$(MSVC_LIB)" INCLUDE="$(MSVC_INCLUDE)"

MSVC64_LIB = $(MSVC_LIB1)/Lib/amd64;$(MSVC_LIB2)/Lib/x64
MSVC64_PREFIX=LIB="$(MSVC64_LIB)" INCLUDE="$(MSVC_INCLUDE)"

MSVCC = $(MSVCC_ROOT)/cl.exe $(MSVC_FLAGS)
MSVCC64 = $(MSVCC_ROOT)/amd64/cl.exe $(MSVC_FLAGS)
else
MSVCC_ROOT:=
MSVC_PREFIX=PATH="$(MSVS_PATH)$(PATH)" LIB="$(MSVS_LIB)$(LIB)" INCLUDE="$(MSVS_INC)$(INCLUDE)"
MSVC64_PREFIX=PATH="$(MSVS64_PATH)$(PATH)" LIB="$(MSVS64_LIB)$(LIB)" INCLUDE="$(MSVS64_INC)$(INCLUDE)"

MSVCC = cl.exe $(MSVC_FLAGS)
MSVCC64 = cl.exe $(MSVC_FLAGS)
endif
endif

show_root:
ifeq ($(MSVCC_ROOT),)
	@echo "$(MSVS_PATH)"
	@echo "$(MSVS_LIB)"
else
	@echo "$(MSVCC_ROOT)"
	@echo "$(MSVC_LIB)"
endif

OCAMLOPT = ocamlopt -g

#OCAMLOPT += -strict-sequence -strict-formats -safe-string -w +A-9

#OCAMLOPT = FLEXLINKFLAGS=-real-manifest ocamlopt
#LINKFLAGS = unix.cmxa

ifeq ($(TOOLCHAIN), msvc)
RES=version.res
ifeq ($(ARCH), i386)
RES_PREFIX=$(MSVC_PREFIX)
else
RES_PREFIX=$(MSVC64_PREFIX)
endif
else
RES=version_res.o
RES_PREFIX=
endif

ifeq ($(NATDYNLINK), false)
#when ocaml is not built with flexlink i.e. -no-shared-libs
LINKFLAGS = -cclib "$(RES)"
else
LINKFLAGS = -cclib "-link $(RES)"
endif

support: $(addprefix build_, $(CHAINS))

build_gnat: flexdll_gnat.o flexdll_initer_gnat.o
build_msvc: flexdll_msvc.obj flexdll_initer_msvc.obj
build_msvc64: flexdll_msvc64.obj flexdll_initer_msvc64.obj
build_cygwin64: flexdll_cygwin64.o flexdll_initer_cygwin64.o
build_mingw: flexdll_mingw.o flexdll_initer_mingw.o
build_mingw64: flexdll_mingw64.o flexdll_initer_mingw64.o

OBJS = version.ml Compat.ml coff.ml cmdline.ml create_dll.ml reloc.ml

COMPILER-$(COMPAT_VERSION):
	$(call RM_F, COMPILER-*)
	$(TOUCH) COMPILER-$(COMPAT_VERSION)

test_ver = $(shell if [ $(COMPAT_VERSION) -ge $(1) ] ; then echo ge ; fi)

# This list must be in order
COMPAT_LEVEL = $(eval COMPAT_LEVEL := \
  $$(strip $$(if $$(call test_ver,40100),401) \
           $$(if $$(call test_ver,40200),402) \
           $$(if $$(call test_ver,40300),403) \
           $$(if $$(call test_ver,40500),405) \
           $$(if $$(call test_ver,40600),406) \
           $$(if $$(call test_ver,40700),407)))$(COMPAT_LEVEL)

Compat.ml: Compat.ml.in COMPILER-$(COMPAT_VERSION)
ifeq ($(SHELL_IS_CMD),)
	sed -e '$(if $(COMPAT_LEVEL),/^$(subst $(SPACE),@\|^,$(COMPAT_LEVEL))@/d;)s/^[0-9]*@//' $< > $@
else
	Compat.cmd $(COMPAT_VERSION) $< > $@
endif

flexlink.exe: $(OBJS) $(RES)
	@echo Building flexlink.exe with TOOLCHAIN=$(TOOLCHAIN) for OCaml $(OCAML_VERSION)
	$(call RM_F, flexlink.exe)
	$(RES_PREFIX) $(OCAMLOPT) -o flexlink.exe $(LINKFLAGS) $(OBJS)

version.res: version.rc
	$(RES_PREFIX) rc version.rc

version_res.o: version.rc
	$(TOOLPREF)windres version.rc version_res.o

flexdll_msvc.obj: flexdll.h flexdll.c
	$(MSVC_PREFIX) $(MSVCC) /DMSVC -c /Fo"flexdll_msvc.obj" flexdll.c

flexdll_msvc64.obj: flexdll.h flexdll.c
	$(MSVC64_PREFIX) $(MSVCC64) /DMSVC /DMSVC64 -c /Fo"flexdll_msvc64.obj" flexdll.c

flexdll_cygwin64.o: flexdll.h flexdll.c
	$(CYG64CC) -c -DCYGWIN -o flexdll_cygwin64.o flexdll.c

flexdll_mingw.o: flexdll.h flexdll.c
	$(MINCC) -c -DMINGW -o flexdll_mingw.o flexdll.c

flexdll_gnat.o: flexdll.h flexdll.c
	gcc -c -o flexdll_gnat.o flexdll.c

flexdll_mingw64.o: flexdll.h flexdll.c
	$(MIN64CC) -c -DMINGW -o flexdll_mingw64.o flexdll.c

flexdll_initer_msvc.obj: flexdll_initer.c
	$(MSVC_PREFIX) $(MSVCC) -c /Fo"flexdll_initer_msvc.obj" flexdll_initer.c

flexdll_initer_msvc64.obj: flexdll_initer.c
	$(MSVC64_PREFIX) $(MSVCC64) -c /Fo"flexdll_initer_msvc64.obj" flexdll_initer.c

flexdll_initer_cygwin64.o: flexdll_initer.c
	$(CYG64CC) -c -o flexdll_initer_cygwin64.o flexdll_initer.c

flexdll_initer_mingw.o: flexdll_initer.c
	$(MINCC) -c -o flexdll_initer_mingw.o flexdll_initer.c

flexdll_initer_gnat.o: flexdll_initer.c
	gcc -c -o flexdll_initer_gnat.o flexdll_initer.c

flexdll_initer_mingw64.o: flexdll_initer.c
	$(MIN64CC) -c -o flexdll_initer_mingw64.o flexdll_initer.c


demo_msvc: flexlink.exe flexdll_msvc.obj flexdll_initer_msvc.obj
	$(MSVC_PREFIX) $(MAKE) -C test clean demo CHAIN=msvc CC="$(MSVCC)" PLUG2_CFLAGS="/bigobj" O=obj

demo_cygwin64: flexlink.exe flexdll_cygwin64.o flexdll_initer_cygwin64.o
	$(MAKE) -C test clean demo CHAIN=cygwin64 CC="$(CYG64CC)" O=o RUN="PATH=\"/cygdrive/c/cygwin64/bin:$(PATH)\""

demo_mingw: flexlink.exe flexdll_mingw.o flexdll_initer_mingw.o
	$(MAKE) -C test clean demo CHAIN=mingw CC="$(MINCC)" O=o

demo_mingw64: flexlink.exe flexdll_mingw64.o flexdll_initer_mingw64.o
	$(MAKE) -C test clean demo CHAIN=mingw64 CC="$(MIN64CC)" O=o

demo_msvc64:  flexlink.exe flexdll_msvc64.obj flexdll_initer_msvc64.obj
	$(MSVC64_PREFIX) $(MAKE) -C test clean demo CHAIN=msvc64 CC="$(MSVCC64)" PLUG2_CFLAGS="/bigobj" O=obj

distclean: clean
	rm -f Makefile.winsdk

clean:
	rm -f *.obj *.o *.lib *.a *.exe *.opt *.cmx *.dll *.exp *.cmi *.cmo *~ version.res version.ml COMPILER-* Compat.ml
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
	cp -a $(filter-out version.ml,$(OBJS:Compat.ml=Compat.ml.in)) Makefile msvs-detect $(COMMON_FILES) version.rc flexdll-$(VERSION)/
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
	    flexlink.exe flexdll_*.obj flexdll_*.o flexdll.c flexdll_initer.c

do_upload_bin:
	rsync $(PACKAGE_BIN) $(URL)

upload_bin: package_bin do_upload_bin

show_toolchain:
	@echo Toolchain for the visible ocamlopt: $(TOOLCHAIN)

swap:
	$(OCAMLOPT) -o flexlink-new.exe $(LINKFLAGS) $(OBJS)
	cp flexlink.exe flexlink.exe.bak
	cp flexlink-new.exe flexlink.exe

PREFIX = "C:\Program Files (x86)\flexdll"

install:
	mkdir -p $(PREFIX)
	cp $(COMMON_FILES) flexlink.exe flexdll_*.obj flexdll_*.o flexdll.c flexdll_initer.c $(PREFIX)

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

AC_INIT(FlexDLL,0.35,alain@frisch.fr,flexdll,http://alain.frisch.fr/flexdll.html)
#AC_COPYRIGHT(Copyright 2016 ?)

AC_ARG_VAR([MINGW_PREFIX],[Prefix for the mingw-w64 i686 cross-compiler])
AC_ARG_VAR([MINGW64_PREFIX],[Prefix for the mingw-w64 x64 cross-compiler])
AS_IF([test x"${MINGW_PREFIX}" = x""],[MINGW_PREFIX=i686-w64-mingw32])
AS_IF([test x"${MINGW64_PREFIX}" = x""],[MINGW64_PREFIX=x86_64-w64-mingw32])

AC_SUBST(MINGW_PREFIX)
AC_SUBST(MINGW64_PREFIX)

# Check for "Cygwin" compilers
CHAINS=
AC_CHECK_TOOL([MINCC],${MINGW_PREFIX}-gcc,[no])
AS_IF([test "${MINCC}" != "no"],[CHAINS=" mingw"])
AC_CHECK_TOOL([MIN64CC],${MINGW64_PREFIX}-gcc,[no])
AS_IF([test "${MIN64CC}" != "no"],[CHAINS="${CHAINS} mingw64"])
AC_CHECK_TOOL([CYGCC],i686-pc-cygwin-gcc,[no])
AS_IF([test "${CYGCC}" != "no"],[CHAINS="${CHAINS} cygwin"])
AC_CHECK_TOOL([CYG64CC],x86_64-pc-cygwin-gcc,[no])
AS_IF([test "${CYG64CC}" != "no"],[CHAINS="${CHAINS} cygwin64"])

# Check for Microsoft compilers
AC_MSG_CHECKING([for an installed pair of Microsoft C Compilers])
MSVS_PATH=no
eval `./msvs-detect --with-assembler --with-mt`
MS_CHAINS=
AS_IF([test "${MSVS_PATH}" = "no"],[
  AC_MSG_RESULT([no])
  AC_MSG_CHECKING([for an installed x86 Microsoft C Compiler])
  eval `./msvs-detect --with-assembler --with-mt --arch=x86`
  AS_IF([test "${MSVS_PATH}" = "no"],[
    AC_MSG_RESULT([no])
  ],[
    AC_MSG_RESULT([${MSVS_NAME}])
    CHAINS="${CHAINS} msvc"
    AC_SUBST(MSVS_PATH)
    AC_SUBST(MSVS_INC)
    AC_SUBST(MSVS_LIB)
  ])
],[
  AC_MSG_RESULT([${MSVS_NAME}])
  CHAINS="${CHAINS} msvc msvc64"
  AC_SUBST(MSVS_PATH)
  AC_SUBST(MSVS_INC)
  AC_SUBST(MSVS_LIB)
  AC_SUBST(MSVS64_PATH)
  AC_SUBST(MSVS64_INC)
  AC_SUBST(MSVS64_LIB)
])
AC_SUBST(CHAINS)

# Check for OCaml
AC_CHECK_TOOL([HAVE_OCAMLOPT],ocamlopt,[no])

AC_CONFIG_FILES(Makefile.config version.ml)
AC_OUTPUT

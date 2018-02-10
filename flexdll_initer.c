/*****************************************************************
   FlexDLL
   Alain Frisch

   Copyright 2007 Institut National de Recherche en Informatique et
   en Automatique.

******************************************************************/

/* Custom entry point to perform relocations before the real
   entry point is called */

/* The adress of the flexdll_relocate function is passed in an
   environment variable. This is ugly, but I couldn't find a cleaner
   solution. Let me know if you have some idea! */

#include <stdlib.h>
#include <stdio.h>
#include <windows.h>

typedef int func(void*);
typedef int func_v2(void*,void*);

extern int reloctbl;
extern int jmptbl;

static int flexdll_init() {
  func *sym = 0;
  func_v2 *sym_v2 = 0;
  char *s = getenv("FLEXDLL_RELOCATE_V2");
  /* If the supplied symbol is NULL, treat as "loaded not for execution" */
  if (!s) {
    s = getenv("FLEXDLL_RELOCATE");
    if (!s) { fprintf(stderr, "Cannot find FLEXDLL_RELOCATE\n"); return FALSE; }
    /* The executable image doesn't support the V2 interface, so RELOC_REL32
       may fail. */
    sscanf(s, "%p", &sym);
    if (!sym || sym(&reloctbl)) return TRUE;
  } else {
    sscanf(s, "%p", &sym_v2);
    if (!sym_v2 || sym_v2(&reloctbl, &jmptbl)) return TRUE;
  }
  return FALSE;
}

#ifdef __GNUC__
#ifdef __CYGWIN__
#define entry  _cygwin_dll_entry
#endif
#ifdef __MINGW32__
#define entry DllMainCRTStartup
#endif
#else
#define entry _DllMainCRTStartup
#endif


BOOL WINAPI entry(HINSTANCE, DWORD, LPVOID);

BOOL WINAPI FlexDLLiniter(HINSTANCE hinstDLL, DWORD fdwReason,
			  LPVOID lpReserved) {
  if (fdwReason == DLL_PROCESS_ATTACH && !flexdll_init())
    return FALSE;

  return entry(hinstDLL, fdwReason, lpReserved);
}

#include <stdlib.h>
#include <stdio.h>

typedef void func(void*);

extern "C" {
  extern int reloctbl;
}

static int flexdll_init() {
  func *sym = 0;
  char *s = getenv("FLEXDLL");
  if (!s) return 0;
  sscanf(s,"%08lx",&sym);
  if (sym) sym(&reloctbl);
  return 0;
}

static int x = flexdll_init();

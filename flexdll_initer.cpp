#include <stdlib.h>
#include <stdio.h>

typedef int func(void*);

extern "C" {
  extern int reloctbl;
}

static int flexdll_init() {
  func *sym = 0;
  char *s = getenv("FLEXDLL");
  if (!s) return 0;
  sscanf(s,"%08lx",&sym);
  if (sym && sym(&reloctbl)) return 0;
  //  throw(0);
    exit(1);
}

static int x = flexdll_init();

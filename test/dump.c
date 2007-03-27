/*****************************************************************
   FlexDLL
   Alain Frisch

   Copyright 2007 Institut National de Recherche en Informatique et 
   en Automatique.

******************************************************************/

/* An example (main program) */

#include <stdlib.h>
#include "flexdll.h"

typedef void torun();

void api(char *msg){ printf("API: %s\n", msg); }

int main(int argc, char **argv)
{
  void *sym;
  void *handle;
  int i;
  torun *torun;

  flexdll_debug = 0;
  for (i = 1; i < argc; i++) {
    handle = flexdll_dlopen(argv[i], 1);
  
    if (NULL == handle) { printf("error: %s\n", flexdll_dlerror()); exit(2); }

    torun = flexdll_dlsym(handle, "torun");
    if (torun) torun();
  }
  exit(0);
}

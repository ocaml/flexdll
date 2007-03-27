

#include <stdlib.h>
#include "flexdll.h"

typedef void torun();

void api(char *msg){
  printf("API: %s\n", msg);
}

extern void *_imp__api;

int main(int argc, char **argv)
{
  void *sym;
  void *handle;
  int i;
  torun *torun;

  printf("DUMP  (__imp_api = %08lx)\n", _imp__api);
  flexdll_debug = 0;
  for (i = 1; i < argc; i++) {
    printf("opening %s\n", argv[i]);
    handle = flexdll_dlopen(argv[i], 1);
  
    if (NULL == handle) { printf("error: %s\n", flexdll_dlerror()); exit(2); }

    torun = flexdll_dlsym(handle, "torun");
    if (torun) torun();
  }
  exit(0);
}

#include <stdlib.h>
#include "dynsyms.h"

typedef void torun();

void api(char *msg){
  printf("API: %s\n", msg);
}

int main(int argc, char **argv)
{
  void *sym;
  void *handle;
  int i;
  torun *torun;

  printf("DUMP\n");
  dyn_debug = 0;
  for (i = 1; i < argc; i++) {
    printf("opening %s\n", argv[i]);
    handle = dyn_dlopen(argv[i], 1);
  
    if (NULL == handle) { printf("error: %s\n", dyn_dlerror()); exit(2); }

    torun = dyn_dlsym(handle, "caml_torun");
    if (torun) {
      printf("Now running...\n",torun);
      torun();
    }
  }

}

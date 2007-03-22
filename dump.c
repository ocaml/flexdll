#include <stdlib.h>
#include "dynsyms.h"

typedef void torun();

int main(int argc, char **argv)
{
  void *sym;
  void *handle;
  int i;
  torun *torun;
  dynsymtable *symtbl = malloc(sizeof(dynsymtable));

  symtbl->size = 0;
  symtbl->used = 0;
  symtbl->sorted = 0;
  symtbl->slots = NULL;

  for (i = 1; i < argc; i++) {
    printf("opening %s\n", argv[i]);
    handle = dlopen(argv[i], 1);
  
    if (NULL == handle) { printf("error: %s\n", dlerror()); exit(2); }

    dump_reloctbl(dlsym(handle, "dynreloc"));
    dump_symtbl(dlsym(handle, "dynsytbl"));

    relocate(find_symbol, symtbl, dlsym(handle,"dynreloc"));
    add_symbols(symtbl, dlsym(handle,"dynsytbl"));

    torun = find_symbol(symtbl, "_caml_torun");
    if (torun) {
      printf("Now running... %08lx\n",torun);
      torun();
    } else {
      printf("No entry point here\n");
    }
  }

}

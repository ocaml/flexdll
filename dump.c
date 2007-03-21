/* Runtime support library for dynamically linking DLLs with
   dynamic symbols */

#include <stdio.h>
#include <string.h>
#include <windows.h>
#include <assert.h>

typedef long intnat;
typedef unsigned long uintnat;

/* Emulate a dlopen-like interface */

void *dlopen(char *libname, int for_execution) {
  HMODULE m;
  m = LoadLibraryEx(libname, NULL,
                    for_execution ? 0 : DONT_RESOLVE_DLL_REFERENCES);
  /* Under Win 95/98/ME, LoadLibraryEx can fail in cases where LoadLibrary
     would succeed.  Just try again with LoadLibrary for good measure. */
  if (m == NULL) m = LoadLibrary(libname);
  return (void *) m;
}

void dlclose(void *handle) { 
  FreeLibrary((HMODULE) handle); 
}

void *dlsym(void *handle, char *name) { 
  return (void *) GetProcAddress((HMODULE) handle, name); 
}

static char * winerror(void)
{
  static char buffer[256];
  DWORD msglen =
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,           /* message source */
                  GetLastError(), /* error number */
                  0,              /* default language */
                  buffer,         /* destination */
                  sizeof(buffer), /* size of destination */
                  NULL);          /* no inserts */
  if (msglen == 0) return "unknown error";
  else return buffer;
}

/* Make a code segment writable */

static void allow_write(char *begin, char *end) {
  static long int pagesize = 0;
  long int old;
  int res;
  SYSTEM_INFO si;

  if (0 == pagesize) {
    GetSystemInfo (&si);
    pagesize = si.dwPageSize;
  }

  begin -= (uintnat) begin % pagesize;
  res = VirtualProtect(begin, end - begin, PAGE_EXECUTE_WRITECOPY, &old);
  if (0 == res) {
    fprintf(stderr, "natdynlink: VirtualProtect failed  %s\n", winerror());
    exit(2);
  }
}

/* Debugging functions */

static void dump_reloctbl(uintnat *ptr) {
  int i,j,n,m;
  uintnat reloc;
  char *name;
  uintnat absolute;

  if (!ptr) { printf("No relocation table\n"); return; }
  printf("Dynamic relocation table found at %lx\n", ptr);
  if ((uintnat) ptr % 4 != 0) { printf("Not aligned\n"); return; }

  if (*ptr++) printf("(already relocated)\n");
  n = *ptr++; 

  for (i = 0; i < n; i++) {
    m = *ptr++; 
    name = (char*) ptr; ptr += 1 + strlen(name) / sizeof(uintnat);
    for (j =0; j < m; j++) {
      reloc = *ptr++;
      absolute = *ptr++;
      printf(" %s: %lx %s  (now:%lx)\n", name, reloc, (absolute ? "(*)" : ""),
	     *((uintnat*) reloc)
	     );
    }
  }
}

static void dump_symtbl(uintnat *ptr)
{
  int i,n;
  uintnat addr;
  char *name;

  if (!ptr) { printf("No relocation table\n"); return; }
  printf("Dynamic symbol table found at %lx\n", ptr);
  if ((uintnat) ptr % 4 != 0) { printf("Not aligned!\n"); return; }

  n = *ptr++;
  for (i = 0; i < n; i++) { 
    addr = *ptr++;
    name = (char*) ptr; ptr += 1 + strlen(name) / sizeof(uintnat);
    printf(" %s : %08lx\n", name, addr);
  }
}

/* Perform relocation */

typedef void *resolver(void*, char*);

void relocate(resolver f, void *data, uintnat *ptr) {
  int i,j,n,m;
  uintnat *reloc, s;
  char *name;
  uintnat absolute;

  assert ((uintnat) ptr % 4 == 0);
  assert (ptr);
  if (*ptr) return;
  *ptr++ = 1;

  n = *ptr++; 
  for (i = 0; i < n; i++) {
    m = *ptr++; 
    name = (char*) ptr; ptr += 1 + strlen(name) / sizeof(uintnat);
    s = (uintnat) f(data,name);
    /*    printf("%s -> 0x%08lx\n", name, s); */
    if (NULL == (void*) s) { printf("Cannot resolve %s\n", name); exit(1); }
    for (j =0; j < m; j++) {
      reloc = (uintnat*) *ptr++;
      absolute = *ptr++;

      allow_write((char*)reloc,(char*)reloc + 4);
      if (absolute) { *reloc += s; }
      else { *reloc = s - (uintnat) reloc - 4; }
    }
  }
}


/* Symbol tables */

typedef struct { void *addr; char *name; } dynsymbol;
typedef struct { 
  int size;    /* Number of allocated slots */
  int used;    /* Number of slots actually used */
  int sorted;  /* 1 if already sorted; 0 otherwise */
  dynsymbol *slots;
} dynsymtable;


int compare_dynsymbol(const void *s1, const void *s2) {
  return strcmp(((dynsymbol*) s1) -> name, ((dynsymbol*) s2) -> name);
}

void add_symbols(dynsymtable *table, uintnat *ptr) {
  int i,used,size;
  int nslots = *ptr++;

  assert ((uintnat) ptr % 4 == 0);
  assert (ptr);

  used = table->used;
  size = table->size;
  if (used + nslots > size) {
    while (used + nslots > size) size = size * 2 + 10;
    table->slots = realloc(table->slots, size * sizeof(dynsymbol));
    if (NULL == table->slots) {
      printf("Cannot allocate memory for symbol table\n");
      exit(1);
    }
  }

  for (i = 0; i < nslots; i++) { 
    dynsymbol *sym = &(table->slots[used++]);
    sym->addr = (void*) *ptr++;
    sym->name = (char*) ptr; ptr += 1 + strlen(sym->name) / sizeof(uintnat);
  }

  table->used = used;
  table->size = size;
  table->sorted = 0;
}

void *find_symbol(dynsymtable *table, char *name) {
  static dynsymbol s;
  dynsymbol *sym;

  if (!table->used) return NULL;
  if (!table->sorted) {
    qsort(table->slots, table->used, sizeof(dynsymbol), &compare_dynsymbol);
    table->sorted = 1;
  }

  s.name = name;
  sym = (dynsymbol*) 
    bsearch(&s,table->slots,table->used, sizeof(dynsymbol),&compare_dynsymbol);

  return (sym ? sym->addr : NULL);
}


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
  
    if (NULL == handle) {
      printf("error: %s\n", winerror());
      exit(2);
    }

    dump_reloctbl(dlsym(handle, "dynreloc"));
    dump_symtbl(dlsym(handle, "dynsytbl"));

    relocate(find_symbol, symtbl, dlsym(handle,"dynreloc"));
    add_symbols(symtbl, dlsym(handle,"dynsytbl"));

    torun = find_symbol(symtbl, "_caml_torun");
    if (torun) {
      printf("Now running...\n");
      torun();
    } else {
      printf("No entry point here\n");
    }
  }

}

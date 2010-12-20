/*****************************************************************
   FlexDLL
   Alain Frisch

   Copyright 2007 Institut National de Recherche en Informatique et
   en Automatique.

******************************************************************/

/* Runtime support library */

#include <stdio.h>
#include <string.h>
#include <windows.h>
#include "flexdll.h"

typedef long intnat;
typedef unsigned long uintnat;

#define RELOC_REL32     0x0001
#define RELOC_ABS       0x0002
#define RELOC_REL32_4   0x0003
#define RELOC_REL32_1   0x0004
#define RELOC_DONE      0x0100

typedef struct { UINT_PTR kind; char *name; UINT_PTR *addr; } reloc_entry;
typedef struct { char *first; char *last; UINT_PTR old; } nonwr;
typedef struct { nonwr *nonwr; reloc_entry entries[]; } reloctbl;
typedef struct { void *addr; char *name; } dynsymbol;
typedef struct { UINT_PTR size; dynsymbol entries[]; } symtbl;
typedef struct dlunit {
  void *handle;
  symtbl *symtbl;
  int global;
  int count;
  struct dlunit *next,*prev;
} dlunit;
typedef void *resolver(void*, const char*);

static int error = 0;
static char error_buffer[256];

/* Emulate a low-level dlopen-like interface */

#ifdef __CYGWIN32__

/* Under Cygwin, use the dlopen interface to allow POSIX paths */

#include <dlfcn.h>

static void *ll_dlopen(const char *libname, int for_execution) {
  return dlopen(libname, RTLD_NOW | RTLD_GLOBAL);
  /* Could use RTLD_LAZY if for_execution == 0, but needs testing */
}

static void ll_dlclose(void * handle)
{
  dlclose(handle);
}

static void * ll_dlsym(void * handle, char * name)
{
  return dlsym(handle, name);
}

static char * ll_dlerror(void)
{
  return dlerror();
}

#else

static void *ll_dlopen(const char *libname, int for_execution) {
  HMODULE m;
  m = LoadLibraryEx(libname, NULL,
		    for_execution ? 0 : DONT_RESOLVE_DLL_REFERENCES);
  /* Under Win 95/98/ME, LoadLibraryEx can fail in cases where LoadLibrary
     would succeed.  Just try again with LoadLibrary for good measure. */
  if (m == NULL) m = LoadLibrary(libname);
  return (void *) m;
}


static void ll_dlclose(void *handle) {
  FreeLibrary((HMODULE) handle);
}

static void *ll_dlsym(void *handle, char *name) {
  return (void *) GetProcAddress((HMODULE) handle, name);
}

static char *ll_dlerror(void)
{
  DWORD msglen =
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,           /* message source */
                  GetLastError(), /* error number */
                  0,              /* default language */
                  error_buffer,         /* destination */
                  sizeof(error_buffer), /* size of destination */
                  NULL);          /* no inserts */
  if (msglen == 0) return "unknown error";
  else return error_buffer;
}

#endif

/** Relocation tables **/

static void dump_reloctbl(reloctbl *tbl) {
  reloc_entry *ptr;
  nonwr *wr;

  if (!tbl) { printf("No relocation table\n"); return; }
  printf("Dynamic relocation table found at %p\n", tbl);

  for (wr = tbl->nonwr; wr->last != 0; wr++)
    printf(" Non-writable relocation in zone %p -> %p\n",
	   wr->first,
	   wr->last);

  for (ptr = tbl->entries; ptr->kind; ptr++)
    printf(" %p (kind:%04lx) (now:%p)  %s\n",
	   ptr->addr,
	   ptr->kind,
	   *((uintnat*) ptr->addr),
	   ptr->name
	   );
}

static void dump_master_reloctbl(reloctbl **ptr) {
  if (!ptr) return;
  while (*ptr) dump_reloctbl(*ptr++);
}

static void allow_write(char *begin, char *end, uintnat new, UINT_PTR *old) {
  static long int pagesize = 0;
  int res;
  SYSTEM_INFO si;

  if (0 == pagesize) {
    GetSystemInfo (&si);
    pagesize = si.dwPageSize;
  }

  begin -= (uintnat) begin % pagesize;
  res = VirtualProtect(begin, end - begin, new, (uintnat*) old);
  if (0 == res) {
    fprintf(stderr, "natdynlink: VirtualProtect failed (%s), begin = 0x%p, end = 0x%p\n", ll_dlerror(), begin, end);
    exit(2);
  }
  /* printf("%p -> %p\n", *old, new); */
}

/* Avoid the use of snprintf */
static void cannot_resolve_msg(char *name) {
  static char msg[] = "Cannot resolve ";
  static int l = sizeof(msg) - 1;
  int n = strlen(name);
  memcpy(error_buffer,msg,l);
  memcpy(error_buffer+l,name,min(n,sizeof(error_buffer) - l - 1));
  error_buffer[l+n] = 0;
}

static void relocate(resolver f, void *data, reloctbl *tbl) {
  reloc_entry *ptr;
  nonwr *wr;
  INT_PTR s;

  if (!tbl) return;

  for (wr = tbl->nonwr; wr->last != 0; wr++)
    allow_write(wr->first,wr->last + 4,PAGE_EXECUTE_WRITECOPY,&wr->old);

  for (ptr = tbl->entries; ptr->kind; ptr++) {
    if (ptr->kind & RELOC_DONE) continue;
    s = (UINT_PTR) f(data,ptr->name);
    if (!s) {
      error = 2;
      cannot_resolve_msg(ptr->name);
      return;
    }
    switch (ptr->kind & 0xff) {
    case RELOC_ABS:
      *(ptr->addr) += s;
      break;
    case RELOC_REL32:
      s -= (INT_PTR)(ptr -> addr) + 4;
      if (s != (INT32) s) {
        printf("flexdll error: cannot relocate, target is too far: %p\n", s);
        fflush(stdout);
        exit(1);
      }
      *((UINT32*) ptr->addr) = s;
      break;
    case RELOC_REL32_4:
      s -= (INT_PTR)(ptr -> addr) + 8;
      if (s != (INT32) s) {
        printf("flexdll error: cannot relocate, target is too far: %p\n", s);
        fflush(stdout);
        exit(1);
      }
      *((UINT32*) ptr->addr) = s;
      break;
    case RELOC_REL32_1:
      s -= (INT_PTR)(ptr -> addr) + 5;
      if (s != (INT32) s) {
        printf("flexdll error: cannot relocate, target is too far: %p\n", s);
        fflush(stdout);
        exit(1);
      }
      *((UINT32*) ptr->addr) = s;
      break;
    default:
      fprintf(stderr, "flexdll: unknown relocation kind");
      exit(2);
    }
    ptr->kind |= RELOC_DONE;
  }

  /* Restore permissions. Should do it also in case of failure... */
  for (wr = tbl->nonwr; wr->last != 0; wr++)
    allow_write(wr->first,wr->last + 4,wr->old,&wr->old);
}

static void relocate_master(resolver f, void *data, reloctbl **ptr) {
  while (0 == error && *ptr) relocate(f,data,*ptr++);
}

/* Symbol tables */

static void dump_symtbl(symtbl *tbl)
{
  int i;

  if (!tbl) { printf("No symbol table\n"); return; }
  printf("Dynamic symbol at %p (size = %i)\n", tbl, tbl->size); fflush(stdout);

  for (i = 0; i < tbl->size; i++) {
    printf("[%i] ", i); fflush(stdout);
    printf(" %p: ", tbl->entries[i].addr); fflush(stdout);
    printf("%s\n", tbl->entries[i].name);
    fflush(stdout);
  }
}

static int compare_dynsymbol(const void *s1, const void *s2) {
  return strcmp(((dynsymbol*) s1) -> name, ((dynsymbol*) s2) -> name);
}

static void *find_symbol(symtbl *tbl, const char *name) {
  static dynsymbol s;
  dynsymbol *sym;

  if (!tbl) return NULL;

  s.name = (char*) name;
  sym =
    bsearch(&s,&tbl->entries,tbl->size, sizeof(dynsymbol),&compare_dynsymbol);

  return (NULL == sym ? NULL : sym -> addr);
}



/* API */

extern symtbl static_symtable;
static dlunit *units = NULL;
static dlunit main_unit;

static void push_unit(dlunit *unit) {
  unit->next = units;
  unit->prev = NULL;
  if (units) units->prev = unit;
  units = unit;
}

static void unlink_unit(dlunit *unit) {
  if (unit->prev) unit->prev->next=unit->next;
  else units=unit->next;

  if (unit->next) unit->next->prev=unit->prev;
}

static void *find_symbol_global(void *data, const char *name) {
  void *sym;
  dlunit *unit;

  if (!name) return NULL;
  sym = find_symbol(&static_symtable, name);
  if (sym) return sym;

  for (unit = units; unit; unit = unit->next) {
    if (unit->global) {
      sym = find_symbol(unit->symtbl, name);
      if (sym) {
	if (unit != units) { unlink_unit(unit); push_unit(unit); }
	return sym;
      }
    }
  }
  return NULL;
}

int flexdll_relocate(void *tbl) {
  if (!tbl) { printf("No master relocation table\n"); return 0; }
  relocate_master(find_symbol_global, NULL, tbl);
  if (error) return 0;
  return 1;
}

void *flexdll_dlopen(const char *file, int mode) {
  void *handle;
  dlunit *unit;
  char flexdll_relocate_env[256];

  int exec = (mode & FLEXDLL_RTLD_NOEXEC ? 0 : 1);

  error = 0;
  if (!file) return &main_unit;

#ifdef MSVC
  sprintf(flexdll_relocate_env,"%p",&flexdll_relocate);
  _putenv_s("FLEXDLL_RELOCATE", flexdll_relocate_env);
#endif
#ifdef CYGWIN
  sprintf(flexdll_relocate_env,"%p",&flexdll_relocate);
  setenv("FLEXDLL_RELOCATE", flexdll_relocate_env, 1);
#endif
#ifdef MINGW
  {
    sprintf(flexdll_relocate_env,"FLEXDLL_RELOCATE=%p",&flexdll_relocate);
    char* s = malloc(strlen(flexdll_relocate_env) + 1);
    strcpy(s, flexdll_relocate_env);
    putenv(s);
  }
#endif

  handle = ll_dlopen(file, exec);
  if (!handle) { if (!error) error = 1; return NULL; }

  unit = units;
  while ((NULL != unit) && (unit->handle != handle)) unit = unit->next;
  if (unit) { unit->count++; }
  else {
    unit = malloc(sizeof(dlunit));
    unit->handle = handle;
    unit->symtbl = ll_dlsym(handle, "symtbl");
    unit->count = 1;
    unit->global = 0;
    push_unit(unit);
  }
  if (mode & FLEXDLL_RTLD_GLOBAL) unit->global=1;

  if (exec) {
    /* Relocation has already been done if the flexdll's DLL entry point
       is used */
    flexdll_relocate(ll_dlsym(handle, "reloctbl"));
    if (error) { flexdll_dlclose(unit); return NULL; }
  }

  return unit;
}

void flexdll_dlclose(void *u) {
  dlunit *unit = u;

  if (NULL == u || u == &main_unit) return;
  ll_dlclose(unit->handle);
  unit->count--;
  if (unit->count == 0) { unlink_unit(unit); free(unit); }
}


void *flexdll_dlsym(void *u, const char *name) {
  if (u == &main_unit) return find_symbol_global(NULL,name);
  else if (NULL == u) return find_symbol(&static_symtable,name);
  else return find_symbol(((dlunit*)u)->symtbl,name);
}

char *flexdll_dlerror() {
  switch (error) {
  case 0: return NULL;
  case 1: error = 0; return ll_dlerror();
  case 2: error = 0; return error_buffer;
  }
  return NULL;
}

void flexdll_dump_exports(void *u) {
  dlunit *unit = u;
  if (NULL == u) { dump_symtbl(&static_symtable); }
  else if (u == &main_unit) {
    dump_symtbl(&static_symtable);
    for (unit = units; unit; unit = unit->next)
      if (unit->global) { dump_symtbl(unit->symtbl); }
  }
  else { dump_symtbl(unit->symtbl); }
}

void flexdll_dump_relocations(void *u) {
  if (NULL == u || u == &main_unit) return;
  dump_master_reloctbl(ll_dlsym(((dlunit*)u) -> handle, "reloctbl"));
}

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
#include <assert.h>
#include "flexdll.h"

typedef long intnat;
typedef unsigned long uintnat;

#define RELOC_REL   0x0001
#define RELOC_ABS   0x0002
#define RELOC_DONE  0x0100

typedef struct { uintnat kind; char *name; uintnat *addr; } reloc_entry;
typedef struct { char *first; char *last; uintnat old; } nonwr;
typedef struct { nonwr *nonwr; reloc_entry entries[]; } reloctbl;
typedef struct { void *addr; char *name; } dynsymbol;
typedef struct { uintnat size; dynsymbol entries[]; } symtbl;
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
  printf("Dynamic relocation table found at %lx\n", tbl);

  for (wr = tbl->nonwr; wr->last != 0; wr++)
    printf(" Non-writable relocation in zone %08lx -> %08lx\n",
	   wr->first,
	   wr->last);
  
  for (ptr = tbl->entries; ptr->kind; ptr++)
    printf(" %08lx (kind:%04lx) (now:%08lx)  %s\n", 
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

static void allow_write(char *begin, char *end, uintnat new, uintnat *old) {
  static long int pagesize = 0;
  int res;
  SYSTEM_INFO si;

  if (0 == pagesize) {
    GetSystemInfo (&si);
    pagesize = si.dwPageSize;
  }

  begin -= (uintnat) begin % pagesize;
  res = VirtualProtect(begin, end - begin, new, old);
  if (0 == res) {
    fprintf(stderr, "natdynlink: VirtualProtect failed (%s), begin = 0x%08lx, end = 0x%08lx\n", ll_dlerror(), begin, end);
    exit(2);
  }
  /* printf("%08lx -> %08lx\n", *old, new); */
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
  uintnat s;

  if (!tbl) return;

  for (wr = tbl->nonwr; wr->last != 0; wr++)
    allow_write(wr->first,wr->last + 4,PAGE_EXECUTE_WRITECOPY,&wr->old);
  
  for (ptr = tbl->entries; ptr->kind; ptr++) {
    if (ptr->kind & RELOC_DONE) continue;
    s = (uintnat) f(data,ptr->name);
    if (!s) { 
      error = 2;
      cannot_resolve_msg(ptr->name);
      return;
    }
    switch (ptr->kind & 0xff) {
    case RELOC_ABS: *(ptr->addr) += s; break;
    case RELOC_REL: *(ptr->addr) = s - (uintnat) (ptr->addr) - 4; break;
    default: assert(0);
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
  printf("Dynamic symbol at %lx\n", tbl);

  for (i = 0; i < tbl->size; i++)
    printf(" %08lx: %s\n", tbl->entries[i].addr, tbl->entries[i].name);
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

void flexdll_relocate(void *tbl) {
  relocate_master(find_symbol_global, NULL, tbl);
  if (error) { 
    fprintf(stderr,"flexdll error: %s\n",flexdll_dlerror());
    exit(1);
  }
}

void *flexdll_dlopen(const char *file, int mode) {
  static int inited  = 0;
  void *handle;
  dlunit *unit;

  int exec = (mode & FLEXDLL_RTLD_NOEXEC ? 0 : 1);
  
  error = 0;
  if (!file) return &main_unit;

  if (!inited) {
    char s[256];
    sprintf(s,"FLEXDLL=%08lx",&flexdll_relocate);
    putenv(s);
    inited = 1;
  }

  handle = ll_dlopen(file, exec);
  if (!handle) { error = 1; return NULL; }

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
    relocate_master(find_symbol_global, NULL,ll_dlsym(handle, "reloctbl")); 
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

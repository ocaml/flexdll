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

/* Guard against compiling with the wrong cl! */
#ifdef MSVC
#if defined(_M_AMD64) && !defined(MSVC64)
#error 64-bit cl selected for a 32-bit build
#elif !defined(_M_AMD64) && defined(MSVC64)
#error 32-bit cl selected for a 64-bit build
#endif
#endif /* MSVC */

typedef long intnat;
typedef unsigned long uintnat;

/* RELOC_ constants except RELOC_DONE have ordinal values based on when
   they were introduced to the code base. These ordinal values are
   persisted in .obj files so do not re-use any existing ordinals. */
#define RELOC_REL32     0x0001
#define RELOC_ABS       0x0002
#define RELOC_REL32_1   0x0004
#define RELOC_REL32_2   0x0005
#define RELOC_REL32_3   0x0008
#define RELOC_REL32_4   0x0003
#define RELOC_REL32_5   0x0006
#define RELOC_32NB      0x0007
#define RELOC_DONE      0x0100

typedef struct { UINT_PTR kind; char *name; UINT_PTR *addr; } reloc_entry;
typedef struct { char *first; char *last; DWORD old; } nonwr;
typedef struct { nonwr *nonwr; reloc_entry entries[]; } reloctbl;
typedef struct { void *addr; char *name; } symtbl_entry;
typedef struct { void *addr; char *name; void *trampoline; } dynsymbol;
typedef struct { UINT_PTR size; symtbl_entry entries[]; } raw_symtbl;
typedef struct { UINT_PTR size; dynsymbol entries[]; } symtbl;
typedef struct dlunit {
  void *handle;
  symtbl *symtbl;
  int global;
  int count;
  struct dlunit *next,*prev;
} dlunit;
typedef dynsymbol *resolver(void*, const char*);

static HANDLE units_mutex = INVALID_HANDLE_VALUE;

/* Error reporting */
/* The latest error must be kept in some variable so that flexdll_dlerror can
 * report it but this causes data races (and possible segmentation faults) in
 * multithreaded programs if that variable is global. So this must use
 * thread-local storage instead.
 *
 * To ensure compatibility, the implementation does not require compiler support
 * for thread-local storage (__thread) and instead relies on functions
 * TlsGetValue, TlsSetValue, etc.
 *
 * This implementation is structured around the function get_tls_error that
 * returns a structure containing, for the current thread, the code (and a
 * buffer for the corresponding message) of the last flexdll error. It accepts
 * an argument to choose whether the structure should be reset:
 *
 * - TLS_ERROR_RESET will reset what is stored in the structure, so this is
 *   intended for initialisation entry points (flexdll_dlopen, flexdll_relocate)
 * - TLS_ERROR_NOP will keep the current content of the structure, for all other
 *   entry points (flexdll_dlerror, ll_dlerror, flexdll_dlsym)
 *
 * The other exported entrypoints do not need to access the error storage.
 */

typedef struct error_s {
  int code;
  char message[256];
} err_t;

#define TLS_ERROR_NOP 0
#define TLS_ERROR_RESET 1

#define TLS_ACCESS_ERRMSG "error accessing thread-local storage"

static err_t *get_tls_error(int op) {
  static volatile DWORD error_idx = TLS_OUT_OF_INDEXES;
  DWORD new_idx, last_error;
  err_t *error;

  /* We store the last system error to restore it on leaving, so that
   * get_tls_error is transparent with regards to GetLastError */
  last_error = GetLastError();

  if(error_idx == TLS_OUT_OF_INDEXES) {
    new_idx = TlsAlloc();
    if(new_idx == TLS_OUT_OF_INDEXES) {
      /* If we cannot allocate the structure required to report errors... */
      /* Maybe we should set the last error to some standard value
       * that correspond to such cases instead of resetting it? */
      SetLastError(last_error);
      return NULL;
    }
    /* According to documentation DWORD and LONG take both 32 bits so
     * this uses InterlockedCompareExchange to store a DWORD */
    if (InterlockedCompareExchange((LONG*)&error_idx, (LONG)new_idx, (LONG)TLS_OUT_OF_INDEXES) != (LONG)TLS_OUT_OF_INDEXES) {
      if(!TlsFree(new_idx)) {
        SetLastError(last_error);
        return NULL;
      }
    }
  }

  error = TlsGetValue(error_idx);
  if(error == NULL) {
    error = malloc(sizeof(err_t));
    if(error == NULL) {
      SetLastError(last_error);
      return NULL;
    }
    if(!TlsSetValue(error_idx, error)) {
      free(error);
      SetLastError(last_error);
      return NULL;
    }
    error->code = 0;
    error->message[0] = 0;
  }

  SetLastError(last_error);

  switch(op) {
  case TLS_ERROR_NOP:
    break;
  case TLS_ERROR_RESET:
    error->code = 0;
    error->message[0] = 0;
    break;
  default:
    return NULL;
  }

  return error;
}

/* Emulate a low-level dlopen-like interface */

#ifdef __CYGWIN__

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

static void *ll_dlopen(const wchar_t *libname, int for_execution) {
  HMODULE m;
  m = LoadLibraryExW(libname, NULL,
                     for_execution ? 0 : DONT_RESOLVE_DLL_REFERENCES);
  /* See https://blogs.msdn.microsoft.com/oldnewthing/20050214-00/?p=36463
     Should use LOAD_LIBRARY_AS_DATAFILE instead of DONT_RESOLVE_DLL_REFERENCES? */

  /* Under Win 95/98/ME, LoadLibraryEx can fail in cases where LoadLibrary
     would succeed.  Just try again with LoadLibrary for good measure. */
  if (m == NULL) m = LoadLibraryW(libname);
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
  DWORD msglen;
  err_t * err;
  err = get_tls_error(TLS_ERROR_NOP);
  if(err == NULL) return TLS_ACCESS_ERRMSG;

  msglen =
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL,                 /* message source */
                  GetLastError(),       /* error number */
                  0,                    /* default language */
                  err->message,         /* destination */
                  sizeof(err->message), /* size of destination */
                  NULL);                /* no inserts */
  if (msglen == 0) return "unknown error";
  else return err->message;
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
	   (void *)ptr->addr,
	   (unsigned long)ptr->kind,
	   (void *)((UINT_PTR)(*((uintnat*) ptr->addr))),
	   ptr->name
	   );
}

static void dump_master_reloctbl(reloctbl **ptr) {
  if (!ptr) return;
  while (*ptr) dump_reloctbl(*ptr++);
}

/* Avoid the use of snprintf */
static void cannot_resolve_msg(char *name, err_t *err) {
  static char msg[] = "Cannot resolve ";
  static size_t l = sizeof(msg) - 1;
  size_t n = strlen(name);
  memcpy(err->message,msg,l);
  memcpy(err->message+l,name,min(n,sizeof(err->message) - l - 1));
  err->message[l+n] = 0;
}

static void relocate(resolver f, void *data, reloctbl *tbl, void **jmptbl, err_t *err) {
  reloc_entry *ptr;
  INT_PTR s;
  DWORD prev_protect;
  static long int page_size = 0;
  SYSTEM_INFO si;
  char *page_start, *page_end;
  char *prev_page_start = (char*)1, *prev_page_end = (char*)1;
  dynsymbol *sym;
  int rel_offset;
  char *reloc_type;
  MEMORY_BASIC_INFORMATION info;

  if (!tbl) return;

  if (0 == page_size) {
    GetSystemInfo (&si);
    page_size = si.dwPageSize;
  }

  for (ptr = tbl->entries; ptr->kind; ptr++) {
    if (ptr->kind & RELOC_DONE) continue;

    sym = f(data, ptr->name);
    if (!sym) {
      err->code = 2;
      cannot_resolve_msg(ptr->name, err);
      goto restore;
    }
    s = (UINT_PTR)sym->addr;

    /* Set up page protection to allow the relocation.  We will undo
       the change on the next relocation if it falls in a different
       page (or at the end of the process), to avoid too many calls to
       VirtualProtect.

       prev_page_start, prev_page_end, prev_protect: describe the
       protection to be reset.

       Note that a single relocation can fall between two pages.
    */

    page_start = (char*)ptr->addr;
    page_end = page_start+((ptr->kind & 0xff) == RELOC_ABS ? sizeof(UINT_PTR) - 1 : 3);
    page_start -= (size_t) page_start % page_size;
    page_end -= (size_t) page_end % page_size;

    if (prev_page_start != page_start || prev_page_end != page_end) {
      if (prev_page_start != (char*)1) {
        /* Restore */
        if (0 == VirtualProtect(prev_page_start, prev_page_end-prev_page_start+page_size, prev_protect, &prev_protect)) {
          fprintf(stderr, "natdynlink: VirtualProtect failed (%s), page_start = 0x%p\n", ll_dlerror(), page_start);
          exit(2);
        }
      }

      prev_page_start = page_start;
      prev_page_end = page_end;
      if (0 == VirtualProtect(page_start, page_end-page_start+page_size, PAGE_READWRITE, &prev_protect)) {
        fprintf(stderr, "natdynlink: VirtualProtect failed (%s), page_start = 0x%p\n", ll_dlerror(), page_start);
        exit(2);
      }
    }

    switch (ptr->kind & 0xff) {
    case RELOC_ABS:
      rel_offset = -1;
      break;
    case RELOC_REL32:
      rel_offset = 4;
      reloc_type = "REL32";
      break;
    case RELOC_REL32_1:
      rel_offset = 5;
      reloc_type = "REL32_1";
      break;
    case RELOC_REL32_2:
      rel_offset = 6;
      reloc_type = "REL32_2";
      break;
    case RELOC_REL32_3:
      rel_offset = 7;
      reloc_type = "REL32_3";
      break;
    case RELOC_REL32_4:
      rel_offset = 8;
      reloc_type = "REL32_4";
      break;
    case RELOC_REL32_5:
      rel_offset = 9;
      reloc_type = "REL32_5";
      break;
    case RELOC_32NB:
      rel_offset = 0;
      reloc_type = "32NB";
      break;
    default:
      fprintf(stderr, "flexdll: unknown relocation kind");
      exit(2);
    }

    if (rel_offset < 0) {
      *(ptr->addr) += s;
    } else {
      if (rel_offset)
        s -= (INT_PTR)(ptr -> addr) + rel_offset;
      s += *((INT32*) ptr -> addr);
retry:
      if (s != (INT32) s) {
        if (!jmptbl) {
          sprintf(err->message, "flexdll error: cannot relocate RELOC_%s, target is too far: %p  %p",
                  reloc_type, (void *)((UINT_PTR) s), (void *) ((UINT_PTR)(INT32) s));
          err->code = 3;
          return;
        }
        if (!sym->trampoline) {
          void* trampoline;
          /* trampolines cannot be created for data */
          if (VirtualQuery(sym->addr, &info, sizeof(info)) && !(info.Protect & 0xf0)) {
            sprintf(err->message, "flexdll error: cannot relocate RELOC_%s, target is too far, and not executable: %p  %p",
                    reloc_type, (void *)((UINT_PTR) s), (void *) ((UINT_PTR)(INT32) s));
            err->code = 3;
            return;
          }
          trampoline = sym->trampoline = *jmptbl;
          /* rex.W jmpq $0x0(%rip) */
          *((__int64*)trampoline) = 0x25ff48;
          /* Place the actual symbol immediately after the instruction */
          *((UINT_PTR*)((char*)trampoline + 7)) = (UINT_PTR)sym->addr;
          /* Pad with nop */
          *(((char*)trampoline + 15)) = 0x90;
          *((UINT_PTR*)jmptbl) += 16;
        }
        s = (UINT_PTR)(sym->trampoline);
        s -= (INT_PTR)(ptr->addr) + rel_offset;
        s += *((INT32*)ptr->addr);
      }
      if (s != (INT32)s) {
        sym->trampoline = NULL;
        goto retry;
      }
      *((UINT32*) ptr->addr) = (INT32)s;
    }
    ptr->kind |= RELOC_DONE;
  }
 restore:
  /* Restore page permission */
  if (prev_page_start != (char*)1) {
    if (0 == VirtualProtect(prev_page_start, prev_page_end-prev_page_start+page_size, prev_protect, &prev_protect)) {
      fprintf(stderr, "natdynlink: VirtualProtect failed (%s), page_start = 0x%p\n", ll_dlerror(), page_start);
      exit(2);
    }
  }
}

static void relocate_master(resolver f, void *data, reloctbl **ptr, void *jmptbl, err_t *err) {
  void **pjmptbl = jmptbl ? &jmptbl : NULL;
  while (0 == err->code && *ptr)
    relocate(f, data, *ptr++, pjmptbl, err);
}

/* Symbol tables */

static void dump_symtbl(symtbl *tbl)
{
  unsigned i;

  if (!tbl) { printf("No symbol table\n"); return; }
  printf("Dynamic symbol at %p (size = %u)\n", tbl, (unsigned int) tbl->size); fflush(stdout);

  for (i = 0; i < tbl->size; i++) {
    printf("[%u] ", i); fflush(stdout);
    printf(" %p: ", tbl->entries[i].addr); fflush(stdout);
    printf("%s\n", tbl->entries[i].name);
    fflush(stdout);
  }
}

static int compare_dynsymbol(const void *s1, const void *s2) {
  return strcmp(((dynsymbol*) s1) -> name, ((dynsymbol*) s2) -> name);
}

static dynsymbol *find_symbol(symtbl *tbl, const char *name) {
  static dynsymbol s;
  dynsymbol *sym;

  if (!tbl) return NULL;

  s.name = (char*) name;
  sym =
    bsearch(&s,&tbl->entries,tbl->size, sizeof(dynsymbol),&compare_dynsymbol);

  return sym;
}



/* API */

extern raw_symtbl static_symtable;
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

static symtbl *augment_symtbl(raw_symtbl *raw_symtbl) {
  symtbl *result;
  dynsymbol *ptr;
  symtbl_entry *src;
  int i;
  result = (symtbl*)malloc(raw_symtbl->size * sizeof(dynsymbol) + sizeof(UINT_PTR));
  ptr = result->entries;
  src = raw_symtbl->entries;
  result->size = raw_symtbl->size;
  i = (int)result->size;
  while (i-- > 0) {
    ptr->addr = src->addr;
    ptr->name = (src++)->name;
    (ptr++)->trampoline = NULL;
  }
  return result;
}

static symtbl *get_static_symtable(void) {
  static symtbl *table = NULL;

  if (table)
    return table;
  else
    return (table = augment_symtbl(&static_symtable));
}

static dynsymbol *find_symbol_global(void *data, const char *name) {
  dynsymbol *sym;
  dlunit *unit;
  (void)data; /* data is unused */

  if (!name) return NULL;
  sym = find_symbol(get_static_symtable(), name);
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

int flexdll_relocate_v2(void *tbl, void *jmptbl) {
  err_t * err;
  err = get_tls_error(TLS_ERROR_RESET);
  if(err == NULL) return 0;

  if (!tbl) { printf("No master relocation table\n"); return 0; }
  relocate_master(find_symbol_global, NULL, tbl, jmptbl, err);
  if (err->code) return 0;
  return 1;
}

int flexdll_relocate(void *tbl) {return flexdll_relocate_v2(tbl, NULL);}

void set_env_ptr(char* name, void* ptr) {
  char env[256];

#if defined(CYGWIN) || __STDC_SECURE_LIB__ >= 200411L
  sprintf(env, "%p", ptr);
#endif

#ifdef CYGWIN
  setenv(name, env, 1);
#elif __STDC_SECURE_LIB__ >= 200411L
  _putenv_s(name, env);
#else
  {
    char* s;
    sprintf(env, "%s=%p", name, relocate);
    s = malloc(strlen(env) + 1);
    strcpy(s, env);
    putenv(s);
  }
#endif
}

#ifdef CYGWIN
void *flexdll_dlopen(const char *file, int mode) {
#else
void *flexdll_wdlopen(const wchar_t *file, int mode) {
#endif
  void *handle;
  dlunit *unit;
  int exec = (mode & FLEXDLL_RTLD_NOEXEC ? 0 : 1);

  err_t * err;
  err = get_tls_error(TLS_ERROR_RESET);
  if(err == NULL) return NULL;
  if (!file) return &main_unit;

  set_env_ptr("FLEXDLL_RELOCATE", (exec ? &flexdll_relocate : 0));
  set_env_ptr("FLEXDLL_RELOCATE_V2", (exec ? &flexdll_relocate_v2 : 0));

again:
  if (units_mutex == INVALID_HANDLE_VALUE) {
    HANDLE hMutex = CreateMutex(NULL, TRUE, NULL);
    if (hMutex == NULL) {
      if (!err->code) err->code = 1;
      return NULL;
    }
    if (InterlockedCompareExchangePointer(&units_mutex, hMutex, INVALID_HANDLE_VALUE) != INVALID_HANDLE_VALUE) {
      CloseHandle(hMutex);
      goto again;
    }
  } else if (WaitForSingleObject(units_mutex, INFINITE) == WAIT_FAILED) {
      if (!err->code) err->code = 1;
      return NULL;
  }

  handle = ll_dlopen(file, exec);
  if (!handle) { if (!err->code) err->code = 1; ReleaseMutex(units_mutex); return NULL; }

  unit = units;
  while ((NULL != unit) && (unit->handle != handle)) unit = unit->next;
  if (unit) { unit->count++; }
  else {
    unit = malloc(sizeof(dlunit));
    unit->handle = handle;
    unit->symtbl = augment_symtbl(ll_dlsym(handle, "symtbl"));
    unit->count = 1;
    unit->global = 0;
    push_unit(unit);
  }
  if (mode & FLEXDLL_RTLD_GLOBAL) unit->global=1;

  if (exec) {
    /* Relocation has already been done if the flexdll's DLL entry point
       is used */
    flexdll_relocate_v2(ll_dlsym(handle, "reloctbl"), ll_dlsym(handle, "jmptbl"));
    if (err->code) { flexdll_dlclose(unit); ReleaseMutex(units_mutex); return NULL; }
  }

  ReleaseMutex(units_mutex);

  return unit;
}

#ifndef CYGWIN

void *flexdll_dlopen(const char *file, int mode)
{
  wchar_t * p = NULL;
  int nbr;
  void * handle;

  err_t * err;
  err = get_tls_error(TLS_ERROR_RESET);
  if(err == NULL) return NULL;

  if (file) {
    nbr = MultiByteToWideChar(CP_THREAD_ACP, 0, file, -1, NULL, 0);
    if (nbr == 0) { if (!err->code) err->code = 1; return NULL; }
    p = malloc(nbr*sizeof(*p));
    MultiByteToWideChar(CP_THREAD_ACP, 0, file, -1, p, nbr);
  }

  handle = flexdll_wdlopen(p, mode);

  if (p) free(p);

  return handle;
}

#endif

void flexdll_dlclose(void *u) {
  dlunit *unit = u;

  if (NULL == u || u == &main_unit) return;
  ll_dlclose(unit->handle);
  unit->count--;
  if (unit->count == 0) { unlink_unit(unit); free(unit->symtbl); free(unit); }
}


void *flexdll_dlsym(void *u, const char *name) {
  dynsymbol *res;
  err_t * err;
  err = get_tls_error(TLS_ERROR_NOP);
  if (err == NULL) return NULL;

  if (WaitForSingleObject(units_mutex, INFINITE) == WAIT_FAILED) {
    if (!err->code) err->code = 1;
    return NULL;
  }
  if (u == &main_unit) res = find_symbol_global(NULL,name);
  else if (NULL == u) res = find_symbol(get_static_symtable(),name);
  else res = find_symbol(((dlunit*)u)->symtbl,name);
  ReleaseMutex(units_mutex);
  return (res ? res->addr : NULL);
}

char *flexdll_dlerror(void) {
  err_t * err;
  err = get_tls_error(TLS_ERROR_NOP);
  if(err == NULL) return TLS_ACCESS_ERRMSG;

  switch (err->code) {
  case 0: return NULL;
  case 1: err->code = 0; return ll_dlerror();
  case 2: err->code = 0; return err->message;
  case 3: err->code = 0; return err->message;
  }
  return NULL;
}

void flexdll_dump_exports(void *u) {
  dlunit *unit = u;
  if (NULL == u) { dump_symtbl(get_static_symtable()); }
  else if (u == &main_unit) {
    dump_symtbl(get_static_symtable());
    for (unit = units; unit; unit = unit->next)
      if (unit->global) { dump_symtbl(unit->symtbl); }
  }
  else { dump_symtbl(unit->symtbl); }
}

void flexdll_dump_relocations(void *u) {
  if (NULL == u || u == &main_unit) return;
  dump_master_reloctbl(ll_dlsym(((dlunit*)u) -> handle, "reloctbl"));
}

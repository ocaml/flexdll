/* Runtime support library for dynamically linking DLLs with
   dynamic symbols */

typedef long intnat;
typedef unsigned long uintnat;

#define RELOC_REL   0x0001
#define RELOC_ABS   0x0002
#define RELOC_DONE  0x0100

typedef struct { uintnat kind; char *name; uintnat *addr; } reloc_entry;
typedef void *resolver(void*, char*);

typedef struct { void *addr; char *name; } dynsymbol;
typedef struct { 
  int size;    /* Number of allocated slots */
  int used;    /* Number of slots actually used */
  int sorted;  /* 1 if already sorted; 0 otherwise */
  dynsymbol *slots;
} dynsymtable;
typedef struct { uintnat size; dynsymbol entries[]; } symtbl;

void *dlopen(char *, int);
void *dlsym(void *, char *);
void dlclose(void *);
char *dlerror(void);
void dump_reloctbl(reloc_entry *);
void relocate(resolver, void *, reloc_entry *);
void dump_symtbl(symtbl *);
void add_symbols(dynsymtable *, symtbl *);
void *find_symbol(dynsymtable *, char *);

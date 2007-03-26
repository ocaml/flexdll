/* Runtime support library for dynamically linking DLLs with
   dynamic symbols */

#define DYN_RTLD_GLOBAL 0x0001
#define DYN_RTLD_LOCAL  0x0002

void *dyn_dlopen(const char *, int);
void *dyn_dlsym(void *, const char *);
void dyn_dlclose(void *);
char *dyn_dlerror(void);

extern int dyn_debug;

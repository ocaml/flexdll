/*****************************************************************
   FlexDLL
   Alain Frisch

   Copyright 2007 Institut National de Recherche en Informatique et 
   en Automatique.

******************************************************************/

/* Header for the runtime support library */

#ifndef FLEXDLL_H
#define FLEXDLL_H

#define FLEXDLL_RTLD_GLOBAL 0x0001
#define FLEXDLL_RTLD_LOCAL  0x0002

void *flexdll_dlopen(const char *, int);
void *flexdll_dlsym(void *, const char *);
void flexdll_dlclose(void *);
char *flexdll_dlerror(void);

extern int flexdll_debug;

#endif

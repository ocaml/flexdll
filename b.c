int caml_y = 3;
void caml_f() { api("B"); printf("In b.c/f()  y=%i\n", caml_y); }
int z = 3;

#include <windows.h>

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved) {
  printf("b.DllMain called. Reason = %i\n", fdwReason);
  return TRUE;
}

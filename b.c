int caml_y = 3;
void caml_f() { printf("In b.c/f()  y=%i\n", caml_y); }

__declspec(dllexport) int z = 3;

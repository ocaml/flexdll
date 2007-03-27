__declspec(dllimport) extern void caml_f();
__declspec(dllimport) extern int caml_y;

void caml_torun() {
  caml_f();
  caml_y = 100;
  caml_f();
}

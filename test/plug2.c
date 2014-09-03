extern int x;

__declspec(dllimport) void api1(char *msg);
__declspec(dllimport) void api2(char *msg);

void torun() {
  api1("plug2.torun();");
  api2("plug2.torun();");
  /*
  dump_x();
  // printf("XXX\n");
  printf("x = %d\n", x);
  x = 100;
  printf("x = %d\n", x);
  dump_x();
  //  printf("XXX\n");
  */
}

extern void dump_x();
extern int x;

void torun() {
  api("plug2.torun();");

  dump_x();
  x = 100;
  dump_x();
}

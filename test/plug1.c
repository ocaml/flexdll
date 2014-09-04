#include "stdio.h"
int x = 10;
void dump_x() { printf("AAA\nx=%i\n", x); }
void torun() { api1("plug1.torun();"); api2("plug1.torun();"); }

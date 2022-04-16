/*
class test {
  public static void main(String[] a) {
    System.out.println(1 h  + 30 s );
  }
}
*/
#include <stdio.h>
#include <math.h> 
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
int main(int argc, char *argv[]) {
  tgc_start(&gc, &argc);
  printf("%d s\n", ((1)*3600 + 30));
  tgc_stop(&gc);

  return 0;
}

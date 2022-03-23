/*
class test {
  public static void main(String[] a) {
    System.out.println(2 g  + 3 kg );
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
  printf("%d g\n", (2 + (3)*1000));
  tgc_stop(&gc);

  return 0;
}

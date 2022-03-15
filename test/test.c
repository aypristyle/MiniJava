/*
class Print42 {
  public static void main(String[] a) {
    if (2 > 1 || 1 > 2) System.out.println(2 mm );
    else System.out.println(0);
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
  if (((2 > 1) || (1 > 2))) printf("%d km\n", 2);
  else printf("%d \n", 0);
  tgc_stop(&gc);

  return 0;
}

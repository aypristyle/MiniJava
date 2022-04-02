/*
class Factorial {
  public static void main(String[] a) {
    {
      System.out.println(new Fac().computeFac(10));
      System.out.println(new Fac().computeFac(42));
    }
  }
}
class Fac {
  public int computeFac(int num) {
    int numAux;
    numAux = 6 / 3;
    return numAux;
  }
}*/
#include <stdio.h>
#include <math.h> 
#include <stdlib.h>
#include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
tgc_t gc;
struct Fac;
void* Fac_computeFac(struct Fac* this, int num);
struct Fac {
  void* (**vtable)();
};
void* (*Fac_vtable[])() = { Fac_computeFac };
void* Fac_computeFac(struct Fac* this, int num) {
  int numAux;
  numAux = (6 / 3);
  return (void*)(numAux);
}
int main(int argc, char *argv[]) {
  tgc_start(&gc, &argc);
  {
    printf("%d \n", ({ struct Fac* tmp1 = ({ struct Fac* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); res->vtable = Fac_vtable; res; }); (int) tmp1->vtable[0](tmp1, 10); }));
    printf("%d \n", ({ struct Fac* tmp1 = ({ struct Fac* res = tgc_calloc(({ extern tgc_t gc; &gc; }), 1, sizeof(*res)); res->vtable = Fac_vtable; res; }); (int) tmp1->vtable[0](tmp1, 42); }));
  }
  tgc_stop(&gc);

  return 0;
}

#include <stdio.h>

extern int fac_tailrec(int);
extern int fac(int);

int main(int argc, char *argv[]) {
    printf("%d", fac(3));
}

#include <stdio.h>

extern int fac_tailrec(int);
extern int fac(int);

int main(int argc, char *argv[]) {
    printf("%d\n", fac_tailrec(3));
    printf("%d", fac(3));
}

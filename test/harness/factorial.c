#include <stdio.h>
#include <stdlib.h>

extern int fac_tailrec(void*, int);
extern int fac(void*, int);

int main(int argc, char *argv[]) {
    void* kptr = malloc(32 * 1024);
    printf("%d\n", fac_tailrec(kptr, 3));
    printf("%d", fac(kptr, 3));
}

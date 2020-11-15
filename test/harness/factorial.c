#include <stdio.h>

extern void kempe_init(void);
extern int fac(int);

int main(int argc, char *argv[]) {
    kempe_init();
    printf("%d", fac(3));
}

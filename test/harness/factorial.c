#include <stdio.h>

extern void kempe_init(void);
extern int factorial(int);

int main(int argc, char *argv[]) {
    printf("%d", factorial(3));
}

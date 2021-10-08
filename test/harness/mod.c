#include <stdio.h>

extern int mod_kmp(int, int);
extern int div_kmp(int, int);

int main(int argc, char *argv[]) {
    printf("%d\n", mod_kmp(2, -3));
    printf("%d\n", div_kmp(2, -3));
}

#include <stdio.h>

extern int k_gcd(int, int);

int main(int argc, char *argv[]) {
    printf("%d", k_gcd(35, 21));
}

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

extern int k_gcd(void*, int, int);
extern bool is_prime(void*, int);

int main(int argc, char *argv[]) {
    void* kptr = malloc(32 * 1024);
    printf("%d\n", is_prime(kptr, 37));
    printf("%d\n", is_prime(kptr, 36));
    printf("%d", k_gcd(kptr, 21, 35));
}

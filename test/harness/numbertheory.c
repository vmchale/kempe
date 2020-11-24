#include <stdbool.h>
#include <stdio.h>

extern int k_gcd(int, int);
extern bool is_prime(int);

int main(int argc, char *argv[]) {
    printf("%d\n", is_prime(37));
    printf("%d\n", is_prime(36));
    printf("%d", k_gcd(21, 35));
}

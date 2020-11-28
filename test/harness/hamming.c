#include <stdint.h>
#include <stdio.h>

extern int hamming(uint64_t, uint64_t);

int main(int argc, char *argv[]) {
    printf("%d", hamming(5, 3));
}

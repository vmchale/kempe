#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern uint64_t from_seed(void*, uint64_t);

int main(int argc, char *argv[]) {
    void* kptr = malloc(32 * 1024);
    printf("%u", (unsigned int) from_seed(kptr, 3012512025));
}

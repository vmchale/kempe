#include <stdint.h>
#include <stdio.h>

extern uint64_t from_seed(uint64_t);

int main(int argc, char *argv[]) {
    printf("%u", (unsigned int) from_seed(3012512025));
}

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

extern bool not(void*, bool);
extern bool eq(void*, bool, bool);

int main(int argc, char *argv[]) {
    void* kptr = malloc(32 * 1024);
    printf("%d\n", not(kptr, true));
    printf("%d\n", not(kptr, false));
    printf("%d", eq(kptr, true, false));
}

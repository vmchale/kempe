#include <stdio.h>
#include <stdlib.h>

extern int id_int(void*, int);

int main(int argc, char *argv[]) {
    void* kptr = malloc(32 * 1024);
    printf("%d", id_int(kptr, 3));
}

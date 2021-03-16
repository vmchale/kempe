#include <stdio.h>

extern int id_int(int);

int main(int argc, char *argv[]) {
    printf("%d", id_int(3));
}

#include <stdio.h>

extern int id0(int);
extern int id1(int);

int main(int argc, char *argv[]) {
    printf("%d\n", id0(4));
    printf("%d", id1(4));
}

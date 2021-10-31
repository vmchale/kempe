#include <stdio.h>

extern int id0(int);
extern int id1(int);
extern int id2(int);
extern int id3(int);

int main(int argc, char *argv[]) {
    printf("%d\n", id0(4));
    printf("%d\n", id1(4));
    printf("%d\n", id2(4));
    printf("%d", id3(4));
}

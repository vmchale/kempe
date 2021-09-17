#include <stdio.h>

extern int succ0(int);
extern int succ1(int);

int main(int argc, char *argv[]) {
    printf("%d", succ0(4));
    printf("%d", succ1(4));
}

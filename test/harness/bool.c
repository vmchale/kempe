#include <stdbool.h>
#include <stdio.h>

extern bool not(bool);

int main(int argc, char *argv[]) {
    printf("%d\n", not(true));
    printf("%d\n", not(false));
}


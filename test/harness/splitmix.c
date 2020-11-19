#include <stdint.h>
#include <stdio.h>

extern uint64_t from_seed(uint64_t);

// modified to have ""multiple return"" since C doesn't really have that
uint64_t next_(uint64_t x, uint64_t* y) {
	uint64_t z = (x += 0x9e3779b97f4a7c15);
	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
	z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
	*y = x;
	return z ^ (z >> 31);
}

int main(int argc, char *argv[]) {
    uint64_t y;
    uint64_t res = next_(3012512025, &y);
    printf("%u\n", (unsigned int) res);
    printf("%u", (unsigned int) from_seed(3012512025));
}
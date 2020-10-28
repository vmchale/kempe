typedef unsigned long int __uint64_t;
typedef __uint64_t uint64_t;

// modified to have ""multiple return"" since C doesn't really have that
uint64_t next(uint64_t x, uint64_t* y) {
	uint64_t z = (x += 0x9e3779b97f4a7c15);
	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
	z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
	*y = x;
	return z ^ (z >> 31);
}

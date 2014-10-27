#include <stdio.h>

const int true = 1;
const int false = 0;

int is_even (long x) {
	return (x % 2) ==0;
}

long  sum_primes (long x) {
	return x;
}

int is_prime (long x) {
	int res = true;
	if (x == 2) {
		res = true;
	} else if (is_even (x)) {
		res = false;
	} else {
		int lim = false;
		long i = 3;
		while ( (! lim) || res ) {
			if ((sqr (i))) {
				
			} else {
				
			}
		}	
	}
	return res;
}

int main () {
	long result = sum_primes (2000000);
	printf("%ld", result);
}
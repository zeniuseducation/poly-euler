#include <stdio.h>
#include <math.h>

const int true = 1;
const int false = 0;

long sum_sieves (int lim) {
	int refs[2000000] = {0};
	int i = 3, llim = ceil(sqrt(lim));
	long res = 0;
	while (i < lim) {
		if ((i <= llim) && (!refs[i])) {
			for (int j = i*i; j < lim; j += (2*i)) {
				refs[j] = true;
			};
		};
		if (!refs[i]) {
			res += i;
		};
		i += 2;
	}
	return 2+res;
}

int nth_sieves (int m, int n) {
	int lim = m * n;
	int refs[lim] ;
	for (int i = 0; i < lim; i++) {
		refs[i] = false;
	};
	int i = 3, llim = ceil(sqrt(lim));
	int res = 2;
	int p = 1;
	while (p < m) {
		if ((i <= llim) && (!refs[i])) {
			for (int j = i*i; j < lim; j += (2*i)) {
				refs[j] = true;
			};
			p++;
			res = i;
		} else if (!refs[i]) {
			p++;
			res = i;	
		};
		
		i += 2;
	}
	return res;
}

long collatz (long n) {
	long tmp = n;
	long res = 1;
	while (tmp != 1) {
		if (0 == (tmp % 2)) {
			tmp = tmp / 2;
			res++;
		} else {
			tmp = 1 + (3 * tmp);
			res++;
		};
	};
	return res;
}

long max_collatz (long start, long lim) {
	long i = start;
	long res = 1;
	long p = 1;
	long temp = 1;
	while (i < lim) {
		temp = collatz (i);
		if (temp > res) {
			p = i;
			res = temp;
		};
		i += 2;
	};
	return p;
};

int main () {
	int result = max_collatz(500001,1000000);
	printf("%d", result);
}
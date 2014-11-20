#include <stdio.h>
#include <math.h>

const int true = 1;
const int false = 0;

long sum_sieves (int lim) {
	int refs[lim];
	for (int i = 3; i < lim; i += 2) {
		refs[i] = true;
	};
	int i = 3, llim = ceil(sqrt(lim));
	long res = 0;
	while (i < lim) {
		if ((i <= llim) && (refs[i])) {
			for (int j = i*i; j < lim; j += (2*i)) {
				refs[j] = false;
			};
		};
		if (refs[i]) {
			res += i;
		};
		i += 2;
	}
	return 2+res;
}

int main () {
	long result = sum_sieves (2000000);
	printf("%ld", result);
}
#include <stdio.h>
#include <math.h>
#include <time.h>

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

int sum_pdivs (int n) {
	int res = 1;
	if (0 == n % 2) {
		int i = 2;
		while (i*i <= n) {
			if (i*i == n) {
				res += i;
			} else {
				if (0 == n % i) {
					res += i + (n / i);
				}
			}
			i++;
		}
	} else {
		int i = 3;
		while (i*i <= n) {
			if (i*i == n) {
				res += i;
			} else {
				if (0 == n % i) {
					res += i + (n / i);
				}
			}
			i += 2;
		}
		
	}
	return res;
}

long sum_amic (long lim) {
	long res = 0;
	long i = 2;
	while (i < lim) {
		long amic = sum_pdivs (i);
		if (amic != i) {
			long div_amic = sum_pdivs (amic);
			if (i == div_amic) {
				res += i;
			}
		}
		i++;
	}
	return res;
}

int non_abundant_sum (int lim) {
	int abundant[lim];
	abundant[1] = false;
	for (int i = 2; i < lim; i++) {
		if (i < sum_pdivs (i)) {
			abundant[i] = true;
		} else {
			abundant[i] = false;
		}
	}	
	
	int res = 0;
	int j = 0;
	int abuns[lim];
	for (int i = 1; i < lim; i++) {
		abuns[i] = true;
	}
	for (int i = 1; i < lim; i++) {
		if (abundant[i]) {
			j = i;
			while ((i+j) < lim) {
				if (abundant[j]) {
					abuns[i+j] = false;
				};
				j++;
			}
		}
		if (abuns[i]) {
			res += i;
		}

	}
	
	return res;
}

int is_prime (long n) {
	int res = true;
	if (n == 1) {
		return false;
	} else if (n == 2) {
		return true;
	} else if (0 == n % 2) {
		return false;
	} else {
		long i = 3;
		while ((i*i <= n) && res) {
			if (0 == n % i) {
				res = false;
			}
			i += 2;
		}
	}
	return res;
}

long next_prime (long n) {
	if (n < 2) {
		return 2;
	} else if (n == 2) {
		return 3;
	} else if (0 == n % 2) {
		if (is_prime (n+1)) {
			return n+1;
		} else {
			return next_prime (n +1);
		}
	} else {
		if (is_prime (2+n)) {
			return 2+n;
		} else {
			return next_prime (2+n);
		}
	}
}

long largest_pfactor (long n) {
	long i = 2;
	long res = 0;
	long divs = n;
	while (i*i <= divs) {
		long rems = divs % i;
		if (0 == rems) {
			divs = divs / i;
			if (is_prime (divs)) {
				return divs;
			} else {
				i = 2;
			}
		} else {
			i = next_prime (i);
		}
	}
}

int main(int argc, char *argv[]) {
	clock_t begin, end;
	double time_spent;

	begin = clock();
	long result = largest_pfactor (600851475143);
	printf("%ld", result);
	end = clock();
	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

	printf("\n%f", time_spent);
}








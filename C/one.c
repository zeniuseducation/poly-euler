#include <stdio.h>
#include <time.h>

const int true = 1;
const int false = 0;

long pita (int lim) {
	long res = 0;
	int a = 3;
	while (res == 0) {
		int b = a + 1;
		int c = lim - (a + b);
		while (b < c && res == 0) {
			if (c*c == (a * a) + (b * b)) {
				res = a * b * c;
			} else {
				b++;
				c = lim - (a + b);
			}
		}
		a++;
	}
	return res;
}

int find_cycle (int n) {
	int refs[n+1];
	for (int i = 0; i < n+1; i++) {
		refs[i] = false;
	}
	int res = 0;
	int i = 1;
	while (!refs[i]) {
		int rems = (10 * i) % n;
		if (0 == rems) {
			return 0;
		} else {
			refs[i] = true;
			i = rems;
			res++;
		}
	}
	return res;
}

int max_cycle (int lim) {
	int n = lim;
	int i = lim;
	int res = 0;
	while (res < i) {
		
		int tmp = find_cycle (i);
		if (tmp > res) {
			res = tmp;
			n = i;
		} 
		i--;
	}
	return n;
}

int main(int argc, char *argv[]) {
	clock_t begin, end;
	double time_spent;

	begin = clock();
	long result = max_cycle (1000);
	printf("%ld", result);
	end = clock();
	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;


	printf("\n%f", time_spent);
}


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
	int refs2[n+1];
	for (int i = 0; i < n+1; i++) {
		refs[i] = false;
		refs2[i] = false;
	}
	int res = 0;
	int res2 = 0;
	int i = 1;
	while (!refs2[i]) {
		int rems = (10 * i) % n;
		if (0 == rems) {
			return 0;
		} else {
			if (refs[i]) {
				refs2[i] = true;
				i = rems;
				res2++;
			} else {
				refs[i] = true;								i = rems;
				res++;
			}
			
		}
	}
	return res2;
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

long euler28 (int lim) {
    long res = 1;
    int a = 6;
    int b = 1;
    int tempa = 6;
    for (int i = 1; i <= lim / 2; i++) {
        res += 4 * a;
        tempa = a + (8 + (a - b));
        b = a;
        a = tempa;
    }
    return res;
}

int main(int argc, char *argv[]) {
	clock_t begin, end;
	double time_spent;

	begin = clock();
	long result = euler28 (1001);
	printf("%ld \n", result);
	end = clock();
	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("\n%f", time_spent);
}


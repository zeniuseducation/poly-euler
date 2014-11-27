#include <stdio.h>
#include <time.h>
#include <math.h>

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

int sumfif (int n) {
    int res = 0, i = n;
    while (i > 0) {
        res += pow (i % 10, 5);
        i = i / 10;
    }
    return res;
}

int euler30 (int lim) {
    int res = 0, i = lim;
    while (i > 10) {
        if (i == sumfif(i)) {
            res += i;
        }
        i--;
    }
    return res;
}

int count_divs (long long n) {
    int res = 2;
    long i = 2;
    if (0 == (n % 2)) {
        while (i*i < n) {
            if (i*i == n) {
                return res++;
            } else {
                if (0 == (n % i)) {
                    res += 2;
                }
            }
            i++;
        }
        return res;
    } else {
        i = 3;
        while (i*i < n) {
            if (i*i == n) {
                return res++;
            } else {
                if (0 == (n % i)) {
                    res += 2;
                }
            }
            i += 2;
        }
        return res;
    }
}

long long triangle500 (int lim) {
    int i = 1000;
    long long val = (1+i) * (i/2);
    while (count_divs (val) <= lim) {
        i++;
        val = (1+i) * (i/2);
    }
    return val;
}



int main(int argc, char *argv[]) {
	clock_t begin, end;
	double time_spent;
	begin = clock();
	long result = triangle500(500);
	printf("%ld \n", result);
	end = clock();
	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("\n%f", time_spent);
}


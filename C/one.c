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

short refsdivs[20000] = {0};

short count_divs ( int n) {
    short res = 2;
    int i = 2;
    int tmp = refsdivs[n];
    if (tmp) {
        return tmp;
    } else {
        if (0 == (n % 2)) {
                while (i*i <= n) {
                    if (i*i == n) {
                        return res++;
                    } else {
                        if (0 == (n % i)) {
                            res += 2;
                        }
                    }
                    i++;
                }
                refsdivs[n] =res;
                return res;
            } else {
                i = 3;
                while (i*i <= n) {
                    if (i*i == n) {
                        return res++;
                    } else {
                        if (0 == (n % i)) {
                            res += 2;
                        }
                    }
                    i += 2;
                }
                refsdivs[n] =res;
                return res;
            }
    }
    
}

int triangle500 (int lim) {
    int i = 3;
    int res = count_divs (i) * count_divs ((i+1)/2);
    while (res <= lim) {
        i++;
        if (0 == i%2) {
            res = count_divs (i/2) * count_divs (i+1);    
        } else {
            res = count_divs (i) * count_divs ((i+1)/2);    
        }
        
    }
    return i*(i+1)/2;
}

int nth_digit (int n, int i) {
    int m = n;
    int j = log10l(n);
    while (j>i) {
        m /= 10;
        j--;
    }
    return m % 10;
}

int count_digits (int n) {
    return n * (pow (10,n) - pow (10 ,(n - 1)));
}

int nth_digits (int i) {
    int n = 0,res=0,cres=0;
    while (i > res) {
        cres = res;
        res += count_digits (n);
        n++;
    }
    return (cres*10) + (n-1);
} 

int nth_champer (int i) {
    if (i<10) {
        return i;
    } else {
        int res = nth_digits (i);
        int fres = res % 10;
        int sres = res / 10;
        int rems = i - sres;
        int prevs, remss, remsss;
        if (0 == rems % fres) {
            prevs = (rems/fres) - 1;
            
        } else {
            prevs = (rems/fres);
        }
        
        remss = rems % fres;
        
        if (0 == remss) {
            remsss = fres -1;
        } else {
            remsss = remss - 1;
        }
        return nth_digit (pow(10,fres-1) + prevs , remsss);
    }
}

int champers (int n) {
    int res = 1;
    for (int i = 0; i < n; i++) {
        res *= nth_champer (pow(10,i));
    }
    return res;
}

int euler1 (int n) {
    int res = 0;
    for (int i = 0; i < n; i++) {
        if ((0 == i % 5) || (0 == i % 3)) {
            res += i;
        }
    }
    return res;
}


int main(int argc, char *argv[]) {
	clock_t begin, end;
	double time_spent;
	begin = clock();
	int result = floor(sqrt(9));
	printf("%d \n", result);
	end = clock();
	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
	printf("\n%f", time_spent);
}


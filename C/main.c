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

int refdivs[50000] = {0};

int sum_pdivs (int n) {
    int tmp = refdivs[n];
    if (tmp != 0) {
        return tmp;
    }
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
    refdivs[n] = res;
	return res;
}

int sum_amic (int lim) {
	int res = 0;
	int i = 2;
	while (i < lim) {
		int amic = sum_pdivs (i);
		if (amic != i) {
			int div_amic = sum_pdivs (amic);
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
	if (n <= 1) {
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

long prev_prime (long n) {
    if (n <= 2) {
        return 0;
    } else if (n == 3) {
        return 2;
    } else if (0 == n % 2) {
        if (is_prime (n-1)) {
            return n-1;
        } else {
            return prev_prime (n-1);
        }
    } else {
        if (is_prime (n-2)) {
            return n-2;
        } else {
            return prev_prime (n-2);
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
    return res;
}

long euler27 (int lim) {
    int b = 2, ra = 0, rb = 2, resb = 0;
    while (b < lim) {
        int a = - lim, cura = a, resa = 1;
        while (a < lim) {
            int n = 1, resn = 1;
            if (a+b+1 > 0) {
                while (is_prime ((n*n)+(a*n)+b)) {
                    n++;
                    resn++;
                }   
                if (resn > resa) {
                    resa = resn;
                    cura = a;
                }             
            }
            a++;

        }
        if (resa > resb) {
            resb = resa;
            ra = cura;
            rb = b;
        }
        b = next_prime(b);
    }
    return ra*rb;
}

long euler27b (int lim) {
    int b = 997, ra = 0, rb = 2, resb = 0;
    while (b > resb) {
        int a = - lim, cura = a, resa = 1;
        while (a < lim) {
            int n = 1, resn = 1;
            if (a+b+1 > 0) {
                while (is_prime ((n*n)+(a*n)+b)) {
                    n++;
                    resn++;
                }   
                if (resn > resa) {
                    resa = resn;
                    cura = a;
                }             
            }
            a += 2;

        }
        if (resa > resb) {
            resb = resa;
            ra = cura;
            rb = b;
        }
        b = prev_prime(b);
    }
    return ra*rb;
}

int cs[8] = {1,2,5,10,20,50,100,200};

long sumas (int i, int c) {
    if (i == 0) {
        return 1;
    } else if (c == 0) {
        return 1;
    } else {
        long res = 0;
        int x = 0;
        while (i >= (x * cs[c])) {
            res += sumas ((i - (x * cs[c])), c-1);
            x++;
        }
        return res;
    }
    
}

long suma_coins (int n) {
    return sumas (n,7);
}

long isuma (int i, int c) {
    if (i == 0) {
        return 1;
    } else if (c == 1) {
        return 1;
    } else {
        long res = 0;
        int x = 0;
        while (i >= (x * c)) {
            res += isuma ((i - (x * c)), c-1);
            x++;
        }
        return res;
    }
    
}

long suma_int (int n) {
    return isuma (n,n-1);
}

int div_count (int i) {
    int res = 0, tmpi =i;
    while (tmpi != 0) {
        res++;
        tmpi /= 10;
    }
    return res;
}

int is_cprime (int i) {
    int m = i, res = div_count(i) - 1, p = res;
    if (i==1 || i==9) {
        return false;
    } else if (i==3 || i==7) {
        return true;
    } else {
        while (p > -1) {
            if (is_prime(m)) {
                p--;
                m = (m / 10) + ((m % 10) * pow(10,res));
            } else {
                return false;
            }
        }
        return true;
    }
}    

int bahan[4] = {1,3,7,9};

int looper (int p, int lim) { 
    int res = 0;
    if (p>lim) {
        return 0;
    } else if (is_cprime(p)) {
        int tmp1 = 1;
        for (int i = 0; i < 4; i++) {
            tmp1 += looper (p*10+(bahan[i]), lim);
        }
        return tmp1;
    } else {
        int tmp2 = 0;
        for (int i = 0; i < 4; i++) {
            tmp2 += looper (p*10 + (bahan[i]), lim);
        }
        return tmp2;
    }
}

int all_cprimes (int lim) {
    int res = 2;
    for (int i = 0; i < 4; i++) {
        res += looper(bahan[i], lim);
    }
    return res;
}

int lt_prime (int p) {
    int lens = 0,tmp = p;
    while (tmp > 10) {
        lens++;
        tmp /= 10;
    }
    int pang = 1;
    for (int i = 0; i < lens; i++) {
        pang *= 10;
    }
    int i = p % pang;
    while (i>10) {
        if (is_prime (i)) {
            pang = 1;
            for (int i = 0; i < lens; i++) {
                pang *= 10;
            }
            i %= pang;
            lens--;
        } else {
            return false;
        }
    }
    return is_prime(i);
}

int rt_prime (int p) {
    int i = p;
    while (i>10) {
        if (is_prime (i)) {
            i /= 10;
        } else {
            return false;
        }
    }
    return is_prime(i);
}

int lprime (int p) {
    return (lt_prime (p) && rt_prime(p));
}

long sum_lsieves (int lim) {
    int refs[1000000] = {0};
    int i = 3, llim = ceil(sqrt(lim));
    long res = 0;
    while (i < lim) {
        if ((i <= llim) && (!refs[i])) {
            for (int j = i*i; j < lim; j += (2*i)) {
                refs[j] = true;
            };
        };
        if (!refs[i]) {
            if (lprime(i)) {
                res += i;
            }
        };
        i += 2;
    }
    return 2+res;
}

long sum_35 (int n) {
    long res = 0;
    for (int i = 0; i < n; i++) {
        if ( (0==(i % 3) )|| (0==(i % 5))) {
            res += i;
        }
    }
    return res;
}

int sum_even_fibo (int lim) {
    int i=1,j=1,k=2, res =0;
    while (i<lim) {
        k = i+j;
        i = k;
        j = i;
        if ((i % 2) == 0) {
            res += i;
        }
    }
    return res;
}


int main(int argc, char *argv[]) {
	clock_t begin, end;
	double time_spent;

	begin = clock();
    long tmp = non_abundant_sum(23000);
	printf("%ld", tmp);
	end = clock();
	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

	printf("\n%f", time_spent);
}








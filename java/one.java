

class Euler {
    
    private static boolean is_prime (long n) {
        if (n < 10) {
            return (n == 2) || (n == 3) || (n==5) || (n==5);
        } else {
            if (0 == (n % 2)) {
                return false;
            } else {
                int i = 3;
                while (i*i <= n) {
                    if (0 == (n % i)) {
                        return false;
                    } else {
                        i += 2;
                    }
                }
                return true;
            }
        }
    }
    
    private static long next_prime (long n) {
        if (n <= 2) {
            return 3;
        } else {
            if (is_prime(n+2) ) {
                return n+2;
            } else {
                return next_prime (n+2);
            }
        }
    }
    
    private static long largest_pfactor (long n) {
        long i=2, divs = n;
        while (! is_prime(divs)) {
            if (0 == divs % i) {
                divs = divs / i;
                i = 2;
            } else {
                i = next_prime (i);
            }
        }
        return divs;
    }
    
    private static int count_divs (int n) {
        short res = 2;
        byte j;
        int i;
        if (0 == (n % 2)) {
            j = 1;
            i = 2;
        } else {
            j = 2;
            i = 3;
        }
        while (i*i <= n) {
            if (i*i == n) {
                return res++;
            } else {
                if (0 == (n % i)) {
                    res += 2;
                }
            }
            i += j;
        }
        return res;
    }
    
    private static int triangle500 (int lim) {
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
    
    private static long sum_sieves (int lim) {
        boolean[] refs = new boolean[lim+1];
        for (int i = 0; i <= lim; i++) {
            refs[i] = false;
        };
        int i = 3;
        long res = 0;
        while (i < lim) {
            if (i*i <= lim) {
                if (!refs[i]) {
                    for (int j = i*i; j < lim; j += (2*i)) {
                        refs[j] = true;
                    };                    
                }

            };
            if (!refs[i]) {
                res += i;
            };
            i += 2;
        }
        return 2+res;
    }

    private static int nth_sieves (int m, int n) {
        int lim = m * n;
        boolean[] refs = new boolean[lim+1] ;
        for (int i = 0; i <= lim; i++) {
            refs[i] = false;
        };
        int i = 3;
        int res = 2;
        int p = 1;
        while (p < m) {
            if ((i*i <= lim) && (!refs[i])) {
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
    
    private static int pow (int a, int m) {
        if (m<=0) {
            return 1;
        } else {
            return a * pow(a,m-1);
        }
    }
    
    private static int numdig (int m) {
        int res = 1;
        int n = m;
        if (m < 10) {
            return 1;
        }
        while (n >= 10) {
            n /= 10;
            res++;
        }
        return res++;
    }
    
    private static int nth_digit (int n, int i) {
        int m = n;
        int j = numdig(n) - 1;
        while (j>i) {
            m /= 10;
            j--;
        }
        return m % 10;
    }

    private static int count_digits (int n) {
        return n * (pow (10,n) - pow (10 ,(n - 1)));
    }

    private static int nth_digits (int i) {
        int n = 0,res=0,cres=0;
        while (i > res) {
            cres = res;
            res += count_digits (n);
            n++;
        }
        return (cres*10) + (n-1);
    } 

    private static int nth_champer (int i) {
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

    private static int champers (int n) {
        int res = 1;
        for (int i = 0; i < n; i++) {
            res *= nth_champer (pow(10,i));
        }
        return res;
    }
    
    private static int euler1 (int n) {
        int res = 0;
        for (int i = 0; i < n; i++) {
            if ((0 == i % 5) || (0 == i % 3)) {
                res += i;
            }
        }
        return res;
    }
    
    private static int sum_even_fibo (int lim) {
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
    
    private static int pita (int lim) {
        int res = 0;
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
    
    public static int sum_pdivs (int n) {
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

    public static int sum_amic (int lim) {
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
    
    public static void main(String[] args) {

        long startTime = System.nanoTime();
        long res = sum_amic(10000);
        long endTime = System.nanoTime();
        long duration = endTime - startTime;
        System.out.println(res);
        long divs = duration/1000000;
        long rems = 1000000 + (duration % 1000000);
        System.out.println(divs +"." +rems+ "ms " + duration/1000 + " microsec");
       
    }
    
    
}


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
        boolean[] refs = new boolean[2000001];
        for (int i = 0; i < lim; i++) {
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
        boolean[] refs = new boolean[lim] ;
        for (int i = 0; i < lim; i++) {
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
    
    public static void main(String[] args) {

        long startTime = System.nanoTime();
        long res = sum_sieves (200000);
        long endTime = System.nanoTime();
        long duration = endTime - startTime;
        System.out.println(res);
        long divs = duration/1000000;
        long rems = 1000000 + (duration % 1000000);
        System.out.println(divs +"." +rems+ "ms");
       
    }
    
    
}
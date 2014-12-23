__author__ = 'macquest'

def even(n) : 0 == n % 2

def helper_prime(p,i) :
    if (i*i) > p :
        return True
    elif 0 == (p % i) :
        return False
    else :
        return helper_prime (p, i+2)

def is_prime (p) :
    if p < 2 :
        return False
    elif p == 2 :
        return True
    elif even(p) :
        return False
    else :
        return helper_prime(p,3)

print(is_prime(10343211))
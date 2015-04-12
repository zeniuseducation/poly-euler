import time

def sol1(lim):
    return sum(range(3,lim,3)) + sum(range(5,lim,5)) - sum(range(15,lim,15))

def sol2r(a,b,lim,res):
    if a > lim :
        return res
    elif a % 2 == 0 :
        return sol2r(a+b,a,lim,res+a)
    else:
        return sol2r(a+b,a,lim,res)

def sol2loop(lim):
    a = 1
    b = 0
    res = 0
    while a < lim :
        if a % 2 == 0:
            res += a
        tmp = a
        a += b
        b = tmp
    return res

def sol2(lim):
    return sol2r(1,0,lim,0)

def is_prime(n) :
    if n < 2 :
        return False
    elif n == 2 :
        return True
    elif n % 2 == 0 :
        return False
    else:
        lim = int(n**0.5)
        i = 3
        while i <= lim :
            if n % i == 0:
                return False
            i += 2
        return True

def sol3(target):
    i = 3
    n = target
    while True:
        while n % i == 0:
            n /= i
        if n == 1:
            return i
        i += 2
        while not is_prime(i):
            i += 2

def is_palin(n) :
    tmp = str(n)
    return list(tmp) == list(reversed(tmp))

def sol4(lim):
    res = 0
    for i in range(lim,800,-1):
        for j in range(i-1,800,-1) :
            tmp = i*j
            if is_palin(tmp) :
                if tmp > res :
                    res = tmp
    return res

def product (listOrArray):
    res = 1
    for i in listOrArray:
        res *= i
    return res

def sol5(lim):
    refs = [0] * (lim+1)
    for i in range(0,lim+1):
        if i == 0:
            refs[i] = 1
        else :
            refs[i] = i

    for i in range(2,lim+1):
        tmpi = refs[i]
        for j in range(i+1,lim+1):
            tmpj = refs[j]
            if tmpj % tmpi == 0:
                refs[j] /= tmpi
    return product(refs)

def sol6(lim) :
    def square (x) :
        return x * x
    return square(sum(range(1,lim+1))) - sum(map(square,range(1,lim+1)))

# This is number 10
def sol10 (lim):
    refs = [True] * (lim+1)
    res = 2
    llim = int (lim**0.5)
    for i in range (3,lim,2):
        if refs [i]:
            if i <= llim:
                for j in range (i*i,lim,2*i):
                    refs [j] = False
                res += i
            else:
                res += i
    return res

# This is problem no 7
def sol7 (lim) :
    llim = 12 * lim
    refs = [True] * (llim * 1)
    ctr = 1
    cur = 2
    lllim = int (llim**0.5)
    for i in range(3,llim,2) :
        if refs [i]:
            if i <= lllim :
                for j in range(i*i,llim,2*i):
                    refs[j]=False
                ctr += 1
                cur = i
            else :
                ctr += 1
                cur = i
                if ctr == lim :
                    return cur



def timex (f,x):
    start_time = time.time()
    tmp = f(x)
    print("--- %s seconds ---" % (time.time() - start_time))
    return tmp
                
                    
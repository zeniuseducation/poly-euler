import time

def sum_sieve (lim):
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

def timex (f,x):
    start_time = time.time()
    tmp = f(x)
    print("--- %s seconds ---" % (time.time() - start_time))
    return tmp
                
                    
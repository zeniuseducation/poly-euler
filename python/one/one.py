import time

def even(n) : 
	return 0 == n % 2

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
		
def iprime (p) :
	if p < 2 :
		return False
	elif p == 2 :
		return True 
	elif even(p) :
		return False 	
	else:
		lim = int(p**0.5)
		i = 3 
		while i <= lim :
			if p % i == 0 :
				return False 
			i += 2
		return True 

def next_prime (p):
	if p == 2 :
		return 3 
	elif even(p):
		if iprime(p+1):
			return p+1 
		else :
			return next_prime(p+1)
	elif iprime(p+2):
		return p+2 
	else :
		return next_prime(p+2)
		
def sol3(target):
	n,i = target,3
	while not iprime(n):
		while 0 == n % i:
			n = n // i 
		i = next_prime(i)
	return n
	
def sol4(start):
	return max([i*j for i in range(start,999) for j in range(start, 999) if i != j])
		
def sol_1 (lim):
	res = 0
	for i in range (2,lim):
		if (0 == i % 3) or (0 == i % 5):
			res += i
	return res
	
def arseq (a,b,lim) : 
	return ((lim//b)/2) * (a+(lim//b)*b)
	
def sol1a (lim) :
	return arseq(3, 3,lim) + arseq(5, 5, lim) - arseq(15, 15, lim)
	
def is_even (a) :
	return 0 == (a % 2)
	
def even_fibo(lim) : 
	a,b,suma = 1,1,0
	while a < lim:
		if is_even(a):
			suma += a
		tmp = a 
		a += b
		b = tmp
	return suma 
			
def sol_10 (lim):
	prime = [True for i in range (lim)]
	llim = int (lim**0.5)
	suma = 2
	for i in range (3,lim,2):
		if prime [i]:
			if i <= llim:
				for j in range (i*i,lim,i*2):
					prime [j]= False
			suma += i
	return suma
			
def timex (f,x):
	start_time = time.time()
	tmp = f(x)
	print("--- %s seconds ---" % (time.time() - start_time))
	return tmp

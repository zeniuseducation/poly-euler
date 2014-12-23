from datetime import datetime

def is_prime (n) :
	if n == 2 :
		return True
	elif 0 == n % 2 :
		return False
	else :
		i = 3
		res = True
		while (i*i <= n) and res :
			if 0 == n % i :
				res = False
				return False
			else :
				i += 2
		return res

def stupid_iteration (lim) :
	my_dict = {}
	for i in range(lim) :
		my_dict[i] = i * i
	for i in range(lim) :
		print my_dict[i]

def find_cycle (n) :
	refs = {}
	refs2 = {}
	res = 0
	res2 = 0
	i = 1
	while (not refs2.get(i,False)) :
		rems = (10 * i) % n
		if rems == 0 :
			return 0
		else :
			if refs.get(i,False) :
				refs2[i] = True
				res2 += 1
				i = rems
			else :
				refs[i] = True
				res += 1
				i = rems
	return res2

def max_cycle (lim) :
	i = lim
	n = lim
	res = 0
	while i > res :
		tmp = find_cycle (i)
		if tmp > res :
			n = i
			res = tmp
		i -= 1
	return [n,res]

start_time = datetime.now()
result = max_cycle (1000)
print result
end_time = datetime.now()
print('Duration: {}'.format(end_time - start_time))

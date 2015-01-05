import Foundation

func odd_prime(p : Int) -> Bool {
    var i  = 3
    while (i * i) <= p {
        if 0 == (p % i) {
            return false
        }
        i += 2
    }
    return true
}

func is_prime (p : Int) -> Bool {
  if p < 2 {
    return false
  } else if p == 2 {
    return true
  } else if 0 == (p % 2) {
    return false
  } else {
    return odd_prime (p)
  }
}

func next_prime (p : Int) -> Int {
  if p < 2 {
    return 2
  } else if p == 2 {
    return 3 
  } else if odd_prime (p + 2) {
    return p + 2 
  } else {
    return next_prime (p+2)
  }
}



func euler3 (p : Int) -> Int {
  var a = 2, b = p 
  while ~is_prime (b) {
    while 0 != (b % a) {
      a = next_prime (a) 
    }
    b /= a 
    a = 2
  }
  return b 
}

func sumPrimes (lim : Int) -> Int {
    var res = 2
    for (var i = 3; i < lim ; i += 2) {
        if odd_prime(i) {
            res += i 
        }
    }
    return res
}

func euler1 (lim : Int) -> Int {
	var res : Int = 0
	for (var i : Int = 1; i < lim ; i++) {
		if (0 == (i % 3)) || (0 == (i % 5)) {
			res += i;
		}
	}
	return res
}

func euler2 (lim : Int) -> Int {
	var a = 1, b  = 2, tmp = 0, res = 0
	while b < lim {
		if 0 == (b % 2) {
			res += b
		}
		tmp = b 
		b += a 
		a = tmp
	}
	return res
}

func euler7a (tar : Int) -> Int {
  var p  = 2 
  for (var i = 1 ; i < tar ; i++) {
    p = next_prime (p)
  }
  return p
}

func sum_sieves (lim : Int) -> Int {
  var refs = [Bool](count: (lim+1), repeatedValue: false)
  var i = 3, llim = Int(sqrt(Double(lim)))
  var res = 0;
  while i < lim {
    if (i <= llim) && (~refs[i]) {
      for (var j = i*i; j < lim; j += (2*i)) {
        refs[j] = true;
      };
    };
    if ~refs[i] {
      res += i;
    };
    i += 2;
  }
  return 2+res;
}

func nth_sieves (m : Int, n : Int) -> Int {
  var lim = m * n;
  var refs = [Bool](count: lim, repeatedValue:false)
  /* for (var i = 0; i < lim; i++) {
    refs[i] = false
  } */
  var i = 3, llim = Int(sqrt(Double(lim)))
  var res = 2, p = 1
  while p < m {
    if (i <= llim) && (~refs[i]) {
      for (var j = i*i; j < lim; j += (2*i)) {
        refs[j] = true
      }
      p++
      res = i
    } else if (~refs[i]) {
      p++;
      res = i;	
    };
    
    i += 2;
  }
  return res;
}

var refsdivs = [Int16](count : 20000, repeatedValue : 0)

func count_divs (n : Int) -> Int16 {
    var res : Int16 = 2
    var i = 2
    var tmp : Int16 = refsdivs[n];
    if tmp != 0 {
        return tmp
    } else {
        if 0 == (n % 2) { 
          while (i*i) <= n {          
            if (i*i) == n {
              return res++
              } else {
                if 0 == (n % i) {
                  res += 2
                  }
                }
                i++
            }
            refsdivs[n] = res
            return res
            } else {
                i = 3
                while (i*i) <= n {
                    if (i*i) == n {
                        return res++;
                    } else {
                        if 0 == (n % i) {
                            res += 2;
                        }
                    }
                    i += 2;
                }
                refsdivs[n] = res
                return res
            }
    }
    
}

func triangle500 (lim : Int16) -> Int {
    var i = 3
    var res = count_divs (i) * count_divs ((i+1)/2)
    while res <= lim {
        i++
        if 0 == (i%2) {
            res = count_divs (i/2) * count_divs (i+1)
        } else {
            res = count_divs (i) * count_divs ((i+1)/2)
        }
        
    }
    return i*(i+1)/2
}

func rcollatz (n : Int) -> Int {
  var tmp = n, res = 1
  while tmp != 1 {
    if 0 == (tmp % 2) {
      tmp = tmp / 2
      res++
    } else {
      tmp = 1 + (3 * tmp)
      res++
    }
  }
  return res
}

var dcol = [Int : Int] ()

func collatz (m : Int) -> Int {
  if m == 1 {
    return 1
  } else {
    var tmp : Int? = dcol[m] 
    if tmp != nil {
      return tmp!
    } else {
      if 0 == (m % 2) {
        tmp = 1 + collatz (m / 2)
        dcol[m] = tmp
        return tmp!
      } else {
        tmp = 1 + collatz (1 + 3 * m)
        dcol[m] = tmp
        return tmp!
      }
    }
  }
}

func max_collatz (start : Int, lim : Int) -> Int {
  var i = start, res = 1, p = 1, temp = 1;
  while i < lim {
    temp = rcollatz (i)
    if (temp > res) {
      p = i
      res = temp
    }
    i += 2
  }
  return p
}

func main () {
    let start = NSDate()
    let result = max_collatz(500001, 1000000)
    let end = NSDate()
    let timeInterval: Double = end.timeIntervalSinceDate(start) * 1000
    println ("Time elapsed \(timeInterval) ms")
    println (result)
}

main()
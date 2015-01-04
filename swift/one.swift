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


func main () {
    let start = NSDate()
    let result = sumPrimes(2000000)
    let end = NSDate()
    let timeInterval: Double = end.timeIntervalSinceDate(start) * 1000
    println ("Time elapsed \(timeInterval) ms")
    println (result)
}

main()
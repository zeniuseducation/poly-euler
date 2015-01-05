import Foundation
import Darwin

func looperPrime (i : Int32, p : Int32) -> Bool {
    if (i*i) > p {
      return true
    } else if 0 == (p % i) {
      return false 
    } else {
      return looperPrime (i + 2, p)
    }
  }

func oddPrime (p : Int32) -> Bool {
  return looperPrime (3, p)
}

func looperSumPrimes (i : Int32, res : Int32, lim : Int32) -> Int32 {
    if i > lim {
      return res 
    } else if oddPrime (i) {
      return looperSumPrimes (i+2, res+i, lim) 
    } else {
      return looperSumPrimes (i+2,res, lim)
    }
}

func sumPrimes (lim : Int32) -> Int32 {

  return looperSumPrimes (3,2, lim)
}

var mdic = [Int : Int] ()

func funFact () -> Bool {
  var jika = mdic[123]
  if jika == nil {
    return true
  } else {
    return false
  }
}

func main () {
    let start = NSDate()
    let result = funFact()
    let end = NSDate()
    let timeInterval: Double = end.timeIntervalSinceDate(start) * 1000
    println ("Time elapsed \(timeInterval) ms")
    println (result)
}

main()
prob1 x y z = (sum [x,(x * 2)..(z - 1)])
              + (sum [y,(y * 2)..(z - 1)]) 
              - (sum [(x * y), ((x * y) * 2)..(z - 1)])

prob2 n = sum (filter even (takeWhile (< n) (map fib [1..])))
  where fib n 
          | n <= 2 = n
          | otherwise = (fib (n - 1)) + (fib (n - 2)) 

prob4 n = maximum (filter (\a -> ((show a) == (reverse (show a)))) (mxx ((length mx) - 1)))
  where mx = reverse [(10 ^ (n - 1))..((10 ^ n) - 1)]
        mxx acc
          | acc == 0 = mx
          | otherwise = (map ((mx !! acc) *) mx) ++ mxx (acc - 1)


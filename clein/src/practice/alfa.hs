numbers = [1,2,3,4,5,6,7,8,9,10]

take' x xs 
  | length xs <= x = xs
  | otherwise = take' x (init xs)

init' xs
  | length xs <= 1 = []
  | otherwise = (head xs) : (init' (tail xs))

drop' x xs
  | x == 0 = xs
  | otherwise = drop' (x - 1) (tail xs)

last' xs 
  | length xs == 1 = head xs
  | otherwise = last' (tail xs)

keep' f xs
  | length xs == 0 = []
  | otherwise = (f $ (head xs)) : (keep' f (tail xs))



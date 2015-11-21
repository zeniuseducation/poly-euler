module Teaching where

gesek n ms mk f 
  | f <= n * ms = (- f, "statis")
  | otherwise = (- (n * mk), "kinetis")


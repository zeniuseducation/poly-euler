import Math
import Data.List

qdHelper :: Int -> Int -> Int
qdHelper d i = if psquare xsqr then xsqr else qdHelper d (succ i)
  where xsqr = 1 + (d * i * i)

-- it returns the smallest x that satisfies x^2 - d.y^2 = 1, for y > 0
quadDiop :: Int -> [Int]
quadDiop d = d : (round . sqrt . fromIntegral $ qdHelper d 1) : []

diops :: Int -> Int -> [Int]
diops m n = [x | x <- [m..n], not . psquare $ x]

solution_1 m n = map quadDiop (diops m n)

-- targeted solutions 97, 109, 139, 149, 151, 157, 166


-- a2 = b2 + 1 => d ( b2 + 1/d)
-- a2 = d.b2 + 1

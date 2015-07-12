module Four where

import Data.List

feq x y = ((y-0.01) <= x) && (x <= (y+0.01))
grad (x,y) = -4*x / y
exit (x,y) = (feq x 0) && (feq y 10)

sol144 p1 p2 = iter p1 p2 0
	where
		iter pp1 pp2 res
			| exit pp2 = res
			| otherwise = iter pp2 (next_impact pp1 pp2) (res + 1)

mungkin lim = iter (0.0 ,10.1) (1.4,-9.6) 1 []
	where
		iter p1 p2 res resti
			| res == lim = resti
			| otherwise = iter p2 (next_impact p1 p2) (res+1) (p1:resti)

next_impact p1@(x1,y1) p2@(x2,y2) = impact_point (next_grad m1 m2) p2
	where
		m1 = reflection_grad p1 p2
		m2 = grad p2

reflection_grad (x1,y1) (x2,y2) = (y2-y1) / (x2-x1)

impact_point m (x,y) = if (feq x1 x) && (feq y1 y) then (x2,y2) else (x1,y1)
	where
		c = y - (m*x)
		bb = 2*m*c
		aa = 4 + m*m
		cc = c*c - 100
		det = sqrt $ (bb*bb) - (4 * aa ** c)
		x1 = ((-bb) + det) / (2 * aa)
		x2 = ((-bb) - det) / (2 * aa)
		y1 = (m*x1) + c
		y2 = (m*x2) + c

next_grad m1 m2 = tan $ (pi - 2*diff) + am1
	where
		am1 = atan m1
		diff = am1 - (atan m2)

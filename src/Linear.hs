module Linear where

import Data.Euclidean


-- A linear diophantine equation 'ax + by = c'
-- where 'x' and 'y' are unknown
data Linear n = Lin { a,b,c :: n }
  deriving
    ( Show, Eq, Ord, Functor
    )

-- Given any arbitrary integer it produces
-- an unique solution (x,y)
type Solution n
   = Maybe (n -> (n, n))


solveLinear :: _ => Linear n -> Solution n
solveLinear Lin {..}
  = [ \k -> (x + k*v, y - k*u) | b /= 0, q == 0 ]
  where
    (d, e) = gcdExt a b
    (h, q) = divMod c d
    f      = div (a*e-d) (-b)
    (x, y) = (e*h, f*h)
    (u, v) = (quot a d, quot b d)

-- Solve and pass input
runLinear l n =
  ($ n) <$> solveLinear l


module Pogo where

import Iso
import Linear
import Data.Euclidean


{-
   You are standing on a pogo stick at the surface of a circle of
   circumferance C. A distance D away from you is a piece of candy.
   Your pogo stick can only jump jumps of some integer length L.
   How many hops H does it take to land on the candy?

   Examples:
     (C=10, D=1, L=2) -> no solution
     (C=23, D=3, L=5) -> 19 jumps
-}

data Pogo i
   = Pogo { l :: i   -- Jump length
          , d :: i   -- Initial distance to the candy
          , c :: i } -- Circle circumference
   deriving
     ( Show, Eq, Ord, Functor
     )


-- How many jumps to get back to the starting point?
roundtrip Pogo {..} =
  div c $ gcd c l

-- H = Number of hops to land on the candy
getH Pogo {..} t =
  div (d + t*c) l

-- T = Number of times passed all the way around the circle
getT Pogo {..} h =
  div (h*l - d) c

-- D = H*L (mod C)
-- D = H*L - T*C


-- Linear: ax + by = c
-- Pogo:   HL - TC = D
isoPogoLin :: Pogo n <-> Linear n
isoPogoLin =
  Iso do \Pogo {l=a, c=b, d=c} -> Lin  {..}
      do \Lin  {a=l, b=c, c=d} -> Pogo {..}

-- Solve for H
solvePogo Pogo {..}
  | g == 1       = Just $ mod x c
  | mod d g /= 0 = Nothing
  | otherwise    = solvePogo $ Pogo (div c g) (div l g) (div d g)
  where
    (g,h) = gcdExt l c
    x     = d*h + c*div (d*h) (-c)


-- When will we first land on the
-- cell at index 'i'?
--
-- Returns a new Pogo such that
-- solving for H gives the answer
--
cell Pogo {..} i
   = Pogo { d = mod (i - d) c, .. }

-- Number of times we have landed on
-- cell 'i' after 't' time
--
countCell p@Pogo {..} i t
  =
    fromMaybe 0
 do
    n <- solvePogo $ cell p i

    let
      v = gcd c l
      g = div q c
      q = t*v
      h = v*n + c + c*g

    -- floor $ g + c/h + q/h
    pure $ div (g*h + c + q) h


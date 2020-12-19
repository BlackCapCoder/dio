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

   D = H*L (mod C)
   D = H*L - T*C
-}

data Pogo i
   = Pogo { l :: i   -- Jump length
          , d :: i   -- Initial distance to the candy
          , c :: i } -- Circle circumference
   deriving
     ( Show, Eq, Ord, Functor
     )

-- H = Number of hops to land on the candy
getH Pogo {..} t =
  div (d + t*c) l

-- T = Number of times passed all the way around the circle
getT Pogo {..} h =
  div (h*l - d) c

-- How many jumps to get back to the starting point?
roundtrip Pogo {..} =
  div c $ gcd c l


-- Solve for H
solvePogo Pogo {..}
  | g == 1       = Just $ mod x c
  | mod d g /= 0 = Nothing
  | otherwise    = solvePogo $ Pogo (div c g) (div l g) (div d g)
  where
    (g,h) = gcdExt l c
    x     = d*h + c*div (d*h) (-c)

-- Pogo is isomorphic to Linear
--
-- Linear: ax + by = c
-- Pogo:   HL - TC = D
--
isoPogoLin :: Pogo n <-> Linear n
isoPogoLin =
  Iso do \Pogo {l=a, c=b, d=c} -> Lin  {..}
      do \Lin  {a=l, b=c, c=d} -> Pogo {..}


-- When will we first land on the
-- cell at index I?
--
-- Returns a new Pogo such that
-- solving for H gives the answer
cell Pogo {..} i
   = Pogo { d = mod (i - d) c, .. }

-- Number of times we have landed on
-- cell I after T time
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

----

{-
  Pogo can emulate fucktoid loops.

  Boolfuck code:

    [ >>> ! >> ]

  Equivalient FOp:

    Jz [Jmp 3, Neg, Jmp 2]

  Equivalient Pogo:

    Pogo { l = 5 -- number of > (minus number of <)
         , d = 3 -- index  of ! }

  Under this interpretation of Pogo ..

    C = length of the memory tape
    L = total distance jumped in a single iteration
    D = distance jumped before the Neg instruction
    H = number of iterations before halting
-}


-- The loop diverges iff False
halts =
  isJust . solvePogo

-- The state of cell I at time T
-- assuming it was False initially
cellState p i t
  = odd $ countCell p i t


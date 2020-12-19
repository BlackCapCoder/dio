module Termination where

import Pogo


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

-----

-- Pogo where we only care about termination
data Termination i (c :: Nat)
   = Term { l, d :: i }

termToPogo t@Term {..}
  = Pogo { c = fi $ natVal t, .. }


-- The result has the same solution H as a Pogo
-- having both candies Da and Db.
--
-- In fucktoid terms: a loop having both
-- Neg instructions.
--
-- `cellState` cannot recover the memory tape
-- of composed Pogos, thus the Term wrapper.
--
instance
  ( Ring i, Euclidean i, Integral i, KnownNat c
  ) => Semigroup (Termination i c)

  where

    Term la da <> Term lb db =

      let f  = solvePogo . termToPogo
          tl = Term (la+lb) da :: Termination i c
          tr = Term (la+lb) db :: Termination i c

      in case (f tl, f tr) of
        (Nothing, _)             -> tr
        (_, Nothing)             -> tl
        (Just a, Just b) | a < b -> tl
                         | let   -> tr


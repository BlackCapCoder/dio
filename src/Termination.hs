module Termination where

import Pogo


-- Pogo but we only care about termination
--
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


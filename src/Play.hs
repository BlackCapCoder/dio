module Play where

import Math.NumberTheory.Euclidean.Coprimes

import Pogo
import Fucktoid


-- [ >>> ! >> ]
--
p1 = Pogo
  { l = 5
  , d = 3
  , c = 21
  }

op1 :: FOp
op1 = pogoOP p1

ex1 :: Tape
ex1 = runFuck (blankTape $ c p1) [op1]


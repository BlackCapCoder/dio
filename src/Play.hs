module Play where

import Math.NumberTheory.Euclidean.Coprimes

import Pogo
import Fucktoid


-- [ >>> ! >> ]
p1 = Pogo
  { l = 5
  , d = 3
  , c = 21
  }

op1 = pogoOp p1

-- 011[1]00011000110001100
ex1 :: Tape
ex1 = runFuck (blankTape $ c p1) [op1]

p2 = Pogo 1 0 21 -- Jnz [Neg, Jmp 1]   [1]11111111111111111111
p3 = Pogo 1 1 21 -- Jnz [Jmp 1, Neg]   0[1]0000000000000000000




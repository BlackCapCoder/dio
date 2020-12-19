-- A small fucktoid turing-tarpit
--
module Fucktoid where

import Data.Semigroup
import ListZipper
import Pogo


data FOp
   = Jmp Int
   | Jnz Program
   | Neg
  deriving
    ( Show, Eq, Ord
    )

type Program
   = [FOp]

type Tape
   = Lz Bit

type Bit
   = Bool

pattern O = False :: Bit
pattern I = True  :: Bit


blankTape :: Int -> Tape
blankTape s = Lz' [] $ replicate s O

runFuck :: Tape -> Program -> Tape =
  foldl $ flip \case
    Neg   -> modify not
    Jnz x -> until copoint (`runFuck` x)
    Jmp n
      | n < 0 -> abs n `times` left
      | let   ->     n `times` right
  where
    times n =
      appEndo #. stimes n .# Endo

runFuck' =
  runFuck $ blankTape 30_000


pogoOP :: Pogo Int -> FOp
pogoOP Pogo {..}
  = Jnz [ Jmp d, Neg, Jmp (l-d) ]

----

bit2chr = \case
  O -> '0'
  I -> '1'

ppTape =
  map bit2chr >>> \(Lz ls x rs) ->
    reverse ls <> ('[' : x : "]") <> rs

pp = putStrLn . ppTape


-- A small fucktoid tarpit
module Fucktoid where

import ListZipper
import Data.Semigroup


data FOp
   = Jmp Int
   | Jz  Program
   | Neg

type Program
   = [FOp]

type Bit  = Bool
type Tape = Lz Bit

blankTape :: Int -> Tape
blankTape s = Lz' [] $ replicate s False

runFuck :: Tape -> Program -> Tape =
  foldr \case
    Neg   -> modify not
    Jz  x -> until copoint (`runFuck` x)
    Jmp n
      | n < 0 -> abs n `times` left
      | let   ->     n `times` right
  where
    times n =
      appEndo .# stimes n #. Endo

runFuck' =
  runFuck $ blankTape 30_000


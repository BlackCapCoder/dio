{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- A small fucktoid turing-tarpit
--
module Fucktoid where

import Data.Semigroup
import Control.Monad.Loops (iterateUntilM)

import ListZipper
import Pogo
import Iso


data FOp i
   = Jmp i
   | Jnz (Program i)
   | Neg
  deriving
    ( Show, Eq, Ord
    , Functor, Foldable
    )

type Program i
   = [FOp i]

type Tape
   = Lz Bit

type Bit
   = Bool

pattern O = False :: Bit
pattern I = True  :: Bit


blankTape :: Int -> Tape
blankTape s = Lz' [] $ replicate s O

runFuck :: _ => Tape -> Program i -> Tape
runFuck =
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

----

isoPogoOp :: _ => Maybe (Pogo a) <-> Maybe (a, FOp a)
isoPogoOp = Iso
  do fmap $ liftA2 (,) c pogoOp
  do (>>= uncurry opPogo)


pogoOp Pogo {..} =
  Jnz $ [ Jmp d | d /= 0 ]
     ++ [ Neg ]
     ++ [ Jmp x | let x = l-d, x /= 0 ]

opPogo :: _ => i -> FOp i -> Maybe $ Pogo i
opPogo c op@(Jnz xs)
  =
  [ Pogo { l = sum op
         , d = sum . Jnz $ takeWhile (/=Neg) xs
         , c }
  | let

  -- A single Neg
  , 1 == length (filter (==Neg) xs)

  -- No layered loops
  , not $ flip any xs \case Jnz _ -> True; _ -> False
  ]

----

bit2chr = \case
  O -> '0'
  I -> '1'

ppTape =
  map bit2chr >>> \(Lz ls x rs) ->
    reverse ls <> ('[' : x : "]") <> rs

pp = putStrLn . ppTape

trace p@(pogoOp->Jnz x) =
  case solvePogo p of
    Nothing -> putStrLn "<<loop>>"
    Just h  -> pp =<< iterateUntilM copoint f e
  where
    e   = blankTape (c p)
    f a = runFuck a x <$ pp a


module Main where

import Test.Microspec

import Pogo
import Fucktoid
import Linear hiding (a,b,c)


main = microspec do

  "solvePogo" ∴
    testSolvePogo

  "solveLinear" ∴ do
    "total" ∴ total $ solveLinear $ Lin 0 0 0

  "Pogo <-> Linear" ∴
    testIso isoPogoLin

  "Pogo <-> FOp" ∴ do
    "opPogo . pogoOp = id" ∴
      liftA2 opPogo c pogoOp =*= Just

    "pogoOp . opPogo = id" ∴
      pending

testIso iso = do
  "con . pro = id" ∴ con iso . pro iso =*= id
  "pro . con = id" ∴ pro iso . con iso =*= id

testSolvePogo = do
  "hand-written cases" ∴ do
    Pogo {l = 2, d = 1, c = 10} =: Nothing
    Pogo {l = 5, d = 3, c = 23} =: Just 19
    Pogo {l = 5, d = 3, c = 21} =: Just 9

  "total" ∴ total do
    solvePogo $ Pogo 0 0 0

  "Neg prefix" ∴ Pogo 1 0 21 =: Just 21 -- Jnz [Neg, Jmp 1] [1]111111..
  "Neg suffix" ∴ Pogo 1 1 21 =: Just 1  -- Jnz [Jmp 1, Neg] 0[1]00000..

  where
    p =: r = it (show p) $ solvePogo p === r


----

infixr 0 ∴
(∴) = describe

infix 4 =*=
(=*=) = liftA2 (===)

----

pogoTup = Iso
  do \(Pogo a b c) -> (a,b,c)
  do \(a,b,c) -> Pogo a b c

linTup = Iso
  do \(Lin a b c) -> (a,b,c)
  do \(a,b,c) -> Lin a b c

instance Arbitrary a => Arbitrary (Pogo a) where
  arbitrary = con pogoTup <$> arbitrary

instance Arbitrary a => Arbitrary (Linear a) where
  arbitrary = con linTup <$> arbitrary


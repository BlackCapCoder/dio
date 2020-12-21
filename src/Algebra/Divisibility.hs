module Algebra.Divisibility
  where

import Algebra.Set
import Algebra.Lattice hiding (top)
import Algebra.PartialOrd
import Algebra.Heyting
import Data.Group
import Data.Semiring
import Math.NumberTheory.Primes


data Divisibility a = Neg a | Zero | One | Pos a
  deriving stock
    ( Show
    )
  deriving
    ( Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice
    , Eq
    -- , Ord
    , PartialOrd, Heyting
    , Semigroup, Monoid, Group
    -- , Semiring, Ring, GcdDomain, Euclidean
    )
    via WrappedSet (Divisibility a)


unDiv = \case
  Zero  -> zero
  One   -> one
  Pos a -> a
  Neg a -> undefined -- product of every number except 'a'

pattern Div :: GcdDomain a => a -> Divisibility a
pattern Div a <- (unDiv->a)
  where Div a = safePos a

-----

toDiv' = \case
  0 -> Zero
  1 -> One
  (-1) -> Zero
  n | n > 0 -> Pos (f n)
    | let   -> Neg (f $ abs n)
  where
    f = factorBack . fmap (fmap $ const 1) . factorise

unDiv' = \case
  Zero  -> 0
  One   -> 1
  Pos a -> a
  Neg a -> -a

pattern Div' :: (Ord a, Num a, UniqueFactorisation a) => a -> Divisibility a
pattern Div' a <- (unDiv'->a)
  where Div' a = toDiv' a

-----

isOne'  = join coprime
isZero' = isOne' . plus one

safeNeg a
  | isOne' a = Zero
  | let      = Neg a

safePos a
  | isOne' a = One
  | let      = Pos a

diff' a b =
  fromMaybe one $ divide a (gcd a b)

subeq' a b =
  isJust $ divide b a

-----


instance GcdDomain a => Set (Divisibility a)
  where

  bot = One
  top = Zero

  isBot = \case
    One -> True
    _   -> False

  isTop = \case
    Zero -> True
    _    -> False

  complement = \case
    Zero  -> One
    One   -> Zero
    Pos a -> Neg a
    Neg a -> Pos a

  union = curry \case
    (Zero,      _) -> Zero
    (_,      Zero) -> Zero
    (One,       b) -> b
    (a,       One) -> a
    (Pos a, Pos b) -> Pos (lcm a b)
    (Neg a, Neg b) -> safeNeg $ gcd   a b
    (Pos a, Neg b) -> safeNeg $ diff' b a
    (Neg a, Pos b) -> safeNeg $ diff' a b

  intersect = curry \case
    (Zero,      b) -> b
    (a,      Zero) -> a
    (One,       _) -> One
    (_,       One) -> One
    (Pos a, Pos b) -> safePos $ gcd a b
    (Neg a, Neg b) -> Neg     $ lcm a b
    (Pos a, Neg b) -> safePos $ diff' a b
    (Neg a, Pos b) -> safePos $ diff' b a

  diff = curry \case
    (One,       _) -> One
    (a,       One) -> a
    (_,      Zero) -> One
    (Zero,  Pos a) -> Neg a
    (Zero,  Neg a) -> Pos a
    (Pos a, Pos b) -> safePos $ diff' a b
    (Neg a, Neg b) -> safeNeg $ diff' b a
    (Neg a, Pos b) -> Neg     $ lcm a b
    (Pos a, Neg b) -> safePos $ gcd a b

  disjoint = curry \case
    (One,       _) -> True
    (_,       One) -> True
    (Zero,      _) -> False
    (_,      Zero) -> False
    (Neg a, Pos b) -> not $ coprime a b
    (Pos a, Neg b) -> not $ coprime a b
    (Pos a, Pos b) -> coprime a b
    (Neg a, Neg b) -> False -- Is this correct?

  subeq = curry \case
    (_,      Zero) -> True
    (Zero,      _) -> False
    (One,       _) -> True
    (_,       One) -> False
    (Pos a, Pos b) -> subeq' a b
    (Neg a, Neg b) -> subeq' b a
    (Pos a, Neg b) -> coprime a b
    (Neg a, Pos b) -> False -- yes?



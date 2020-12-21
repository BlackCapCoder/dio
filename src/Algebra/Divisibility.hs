{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Divisibility
  ( Divisibility (..)

  , pattern Div
  , pattern Div'
  , divisibility,   divisibility'
  , unDivisibility, unDivisibility'

  , singleton
  , insert
  , delete
  , member
  , toList
  )
  where

import Prelude hiding (toList)

import Algebra.Set
import Algebra.Lattice hiding (top)
import Algebra.PartialOrd
import Algebra.Heyting
import Data.Group
import Data.Semiring
import Math.NumberTheory.Primes

import Data.DistinctFactors
import Data.MonoTraversable
import Data.Containers qualified as M


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

----

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

----

-- Remove prime factors that appear more than once
uniqueFactors =
  getDistinct . distinct

divisibility n
  | isZero' n = Zero
  | isOne'  n = One
  | let       = Pos $ uniqueFactors n

unDivisibility = \case
  Zero  -> zero
  One   -> one
  Pos a -> a
  Neg a -> undefined -- product of every prime / a

pattern Div :: (GcdDomain a, UniqueFactorisation a) => a -> Divisibility a
pattern Div a <- (unDivisibility->a)
  where Div a = divisibility a

-----

divisibility' = \case
  0    -> Zero
  (-1) -> Zero
  1    -> One
  n | n > 0 -> Pos (uniqueFactors n)
    | let   -> Neg (uniqueFactors $ abs n)

unDivisibility' = \case
  Zero  -> 0
  One   -> 1
  Pos a -> a
  Neg a -> -a

pattern Div' :: (Ord a, Num a, UniqueFactorisation a) => a -> Divisibility a
pattern Div' a <- (unDivisibility'->a)
  where Div' a = divisibility' a

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


safeNeg' a
  | isOne'  a = Zero
  | isZero' a = One
  | let       = Neg a

safePos' a
  | isOne'  a = One
  | isZero' a = Zero
  | let       = Pos a

-----

singleton  = safePos' . getDistinct
insert p s = s ∪ singleton p
delete p s = s ∖ singleton p
member p s = singleton p ⊆ s

toList :: _ => Divisibility a -> [Prime a]
toList = \case
  One   -> []
  Zero  -> primes
  Pos a -> fst <$> factorise a
  Neg a -> filter (coprime a . unPrime) primes

----

type instance Element (Divisibility a)
   = DistinctFactors a

instance GcdDomain a => MonoPointed (Divisibility a) where
  opoint = singleton

instance
  ( Integral a
  , UniqueFactorisation a
  , GcdDomain a
  ) => MonoFoldable (Divisibility a)
  where

    ofoldr f e =
      foldr (f . fromPrime) e . toList

    ofoldl' f e =
      foldl' (\a -> f a . fromPrime) e . toList

    ofoldMap f =
      foldMap (f . fromPrime) . toList

    ofoldl1Ex' f =
      ofoldl1Ex' f . map fromPrime . toList

    ofoldr1Ex f =
      ofoldr1Ex f . map fromPrime . toList

instance
  ( Integral a
  , UniqueFactorisation a
  , GcdDomain a
  ) => GrowingAppend (Divisibility a)

instance
  ( Eq (DistinctFactors a)
  , Integral a
  , UniqueFactorisation a
  , GcdDomain a
  ) => M.SetContainer (Divisibility a)
  where
    type ContainerKey (Divisibility a) = DistinctFactors a

    member        = member
    notMember a b = not $ member a b
    union         = union
    difference    = diff
    intersection  = intersect
    keys          = map fromPrime . toList

instance
  ( Eq (DistinctFactors a)
  , Integral a
  , UniqueFactorisation a
  , GcdDomain a
  ) => M.IsSet (Divisibility a)
  where
    insertSet    = insert
    deleteSet    = delete
    singletonSet = singleton
    setFromList  = safePos' . product . map getDistinct
    setToList    = M.keys

    -- This could definitly be more clever,
    -- but I don't know if it's actually useful!
    --
    filterSet f = \case
      One -> One
      Pos a | f (unsafeDistinct a) -> Pos a
      a -> M.setFromList . filter f $ M.setToList a


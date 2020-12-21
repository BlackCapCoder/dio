module Algebra.Divisibility
  where

import Algebra.Set
import Algebra.Lattice hiding (top)
import Algebra.PartialOrd
import Algebra.Heyting
import Data.Group


newtype Divisibility a = Divisibility { undiv :: a }
  deriving stock
    ( Show
    )
  deriving
    ( Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice
    , Eq, Ord, PartialOrd, Heyting
    , Semigroup, Monoid, Group
    , Semiring, Ring, GcdDomain, Euclidean
    )
    via WrappedSet (Divisibility a)


divide' :: ∀ a d. (GcdDomain a, d ~ Divisibility a) => d -> d -> Maybe d
divide' = coerce do divide @a

instance GcdDomain a => Set (Divisibility a)
  where
    bot       = coerce do one     @a
    top       = coerce do zero    @a
    union     = coerce do lcm     @a
    intersect = coerce do gcd     @a
    disjoint  = coerce do coprime @a

    isBot =
      join disjoint

    isTop =
      isBot . Divisibility . plus one .# undiv

    subeq b a
      | isTop a = isTop b -- avoid division by zero
      | let     = isJust $ divide' b a

    -- This is incorrect.
    --
    -- The problem is that we consider 0 to "contain everything",
    -- which is fine until we have to remove something from it
    -- and produce a value that contains everything except X.
    --
    -- We could use the negative numbers for this at the cost
    -- of another constraint?
    --
    diff a b =
      fromMaybe bot $ divide' a (a ∩ b)


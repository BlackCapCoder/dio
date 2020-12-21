module Algebra.Divisibility
  where

import Algebra.Set
import Algebra.Lattice hiding (top)
import Algebra.PartialOrd
import Algebra.Heyting
import Data.Group


type D = Divisibility

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


divide' :: ∀ a. GcdDomain a => D a -> D a -> Maybe (D a)
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

    diff a b =
      fromMaybe bot $ divide' a (a ∩ b)

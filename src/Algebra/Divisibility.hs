module Algebra.Divisibility
  where

import Algebra.Set
import Algebra.Lattice hiding (top)
import Algebra.PartialOrd
import Algebra.Heyting


newtype Divisibility a = Divisibility a
  deriving newtype
    ( Semiring, GcdDomain
    , Euclidean, Ring, Num, Show
    )
  deriving
    ( Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice
    , Eq, PartialOrd
    ) via WrappedSet (Divisibility a)

instance GcdDomain a => Set (Divisibility a)
  where
    bot       = one
    top       = zero
    union     = lcm
    intersect = gcd
    disjoint  = coprime

    isBot =
      join disjoint

    isTop =
      isBot . plus one

    subeq b a
      | isTop a = isTop b -- avoid division by zero
      | let     = isJust $ divide b a

    diff a b =
      fromMaybe bot $ divide a (a ∩ b)


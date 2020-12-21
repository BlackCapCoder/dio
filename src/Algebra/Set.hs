module Algebra.Set
  ( module Algebra.Set
  )
  where


import Algebra.Lattice hiding (top)
import Algebra.Lattice qualified
import Algebra.PartialOrd
import Algebra.Heyting

import Data.Group
import Data.Semigroup.Cancellative
import Data.Monoid.Null   as M
import Data.Monoid.Monus  as M
import Data.Monoid.GCD    as M

import Data.Semiring
import Data.Euclidean



class Set s
  where

  union      :: s -> s -> s
  intersect  :: s -> s -> s

  top        :: s
  bot        :: s
  isTop      :: s -> Bool
  isBot      :: s -> Bool

  subeq      :: s -> s -> Bool
  equal      :: s -> s -> Bool
  disjoint   :: s -> s -> Bool

  uniq       :: s -> s -> s
  diff       :: s -> s -> s

  complement :: s -> s

  subeq a b = (a ∪ b) ≡ b
  equal a b = (a ⊆ b) ∧ (b ⊆ a)

  isTop = subeq top
  isBot = join disjoint

  disjoint a b =
    (a ∩ b) ⊆ bot

  uniq a b = (a ∪ b) ∖ (a ∩ b)
  diff a b = a ∩ uniq a b

  complement a =
    top ∖ a

----

supeq = flip subeq

(∩) = intersect
(∪) = union
(⊆) = subeq
(⊇) = supeq
(≡) = equal
(∖) = diff; (\\) = diff

(∧) = (/\)
(∨) = (\/)

infixr 3 ∩, ∧
infixr 2 ∪, ∨
infix  4 ≡

pattern Top <- (isTop->True)
  where Top = top

pattern Bot <- (isBot->True)
  where Bot = bot

----

newtype WrappedSet a = WrappedSet a
  deriving newtype Set


instance Set a => Lattice (WrappedSet a) where
  (/\) = (∩)
  (\/) = (∪)

instance Set a => BoundedJoinSemiLattice (WrappedSet a) where bottom = bot
instance Set a => BoundedMeetSemiLattice (WrappedSet a) where top    = top

instance Set a => Eq         (WrappedSet a) where (==) = (≡)
instance Set a => PartialOrd (WrappedSet a) where leq  = (⊆)

instance Set a => Heyting (WrappedSet a) where
  neg     = complement
  x ==> y = neg x ∪ y

instance Set a => Semigroup  (WrappedSet a) where (<>)   = (∪)
instance Set a => Monoid     (WrappedSet a) where mempty = bot
instance Set a => Unit       (WrappedSet a) where unit   = bot
instance Set a => MonoidNull (WrappedSet a) where null   = isBot
instance Set a => Group      (WrappedSet a) where invert = complement
instance Set a => Monus      (WrappedSet a) where (<\>)  = (∖)

instance Set a => LeftGCDMonoid  (WrappedSet a) where commonPrefix = (∩)
instance Set a => RightGCDMonoid (WrappedSet a) where commonSuffix = (∩)
instance Set a => GCDMonoid      (WrappedSet a) where gcd          = (∩)

instance Set a => Abelian        (WrappedSet a)
instance Set a => Commutative    (WrappedSet a)
instance Set a => PositiveMonoid (WrappedSet a)

instance Set a => LeftReductive (WrappedSet a) where
  isPrefixOf      = (⊆)
  stripPrefix a b = b </> a

instance Set a => RightReductive (WrappedSet a) where
  isSuffixOf      = (⊆)
  stripSuffix a b = b </> a

instance Set a => Reductive (WrappedSet a) where
  (</>) a b = (a ∖ b) <$ guard (b ⊆ a)

instance Set a => OverlappingGCDMonoid (WrappedSet a) where
  stripOverlap a b = (a ∖ b, a ∩ b, b ∖ a)

-- instance Set a => Ord (WrappedSet a) where
--   a <= b = isTop $ a ==> b
--
-- instance Set a => Semiring (WrappedSet a) where
--   zero  = bot
--   one   = top
--   plus  = (∪)
--   times = (∩)
--
-- instance Set a => Ring (WrappedSet a) where
--   negate = neg
--
-- instance Set a => GcdDomain (WrappedSet a) where
--   gcd        = (∩)
--   lcm        = (∪)
--   coprime    = disjoint
--   divide a b = (a ∖ b) <$ guard (b ⊆ a)
--
-- instance Set a => Euclidean (WrappedSet a) where
--   quot   = diff
--   rem    = uniq
--   degree = isoSemi
--
-- isoSemi :: (Ord a, Semiring a, Semiring b) => a -> b
-- isoSemi a
--   | isZero a  = zero
--   | otherwise = go zero (one, one)
--   where
--     go r (b, n)
--       | a >= b+b+r = go r (b+b, n+n)
--       | a == b+r   = n
--       | otherwise  = go (r+b) (one, one) + n


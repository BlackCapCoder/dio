{-# LANGUAGE NoArrows #-}
module Data.QSet where

import Prelude hiding (filter)

import Iso
import Data.Unfoldable
import Control.Search qualified as S
import Control.Search (SearchT)

import Data.Functor.Apply
import Data.Functor.Bind as B
import Data.Witherable.Class
import Data.Group

import Algebra.Set
import Algebra.Lattice hiding (top)
import Algebra.Lattice qualified
import Algebra.PartialOrd
import Algebra.Heyting


newtype QSet a
      = QSet { toSearch :: SearchT Maybe () a }
  deriving newtype
    ( Pointed
    , Functor, Apply, Applicative, Monad
    , Alternative, MonadPlus, MonadFail
    , Filterable, Semialign, Zip
    )
  deriving
    ( Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice
    , Eq
    , PartialOrd, Heyting
    , Group
    )
    via WrappedSet (QSet a)


fromSearch :: SearchT Maybe () a -> QSet a
fromSearch = coerce

qset :: ∀ a. ((a -> Bool) -> Maybe a) -> QSet a
qset = QSet . S.searchT . q
  where
    q f g = f $ pro isoMUnitBool . g

find :: QSet a -> (a -> Bool) -> Maybe a
find = toSearch
   >>> S.runSearchT
   >>> \f p -> f $ con isoMUnitBool . p

----

isoMUnitBool :: Maybe () <-> Bool
isoMUnitBool =
  Iso isJust \x -> () <$ guard x

isoQSetSearch :: QSet a <-> SearchT Maybe () a
isoQSetSearch =
  Iso coerce coerce

----

instance Semigroup (QSet a) where
  s <> t = B.join (pair s t)

instance Monoid (QSet a) where
  mempty = qset $ const Nothing

instance Bind QSet where
  join ss = qset \p ->
    flip find p -<< find ss \s -> forsome s p

instance Unfoldable QSet where
  unfold = foldr insert mempty

instance (Show a, Eq a) => Show (QSet a) where
  show = show . setToList

instance Eq a => Set (QSet a)
  where
    bot
      = mempty

    union
      = (<>)

    intersect a b
      = filter (contains b) a

    subeq a b
      = forevery a $ contains b

    diff a b
      = filter (not . contains b) a

    top
      = undefined

----

forsome :: QSet a -> (a -> Bool) -> Bool
forsome s p = maybe False p (find s p)

forevery :: QSet a -> (a -> Bool) -> Bool
forevery s p = not (forsome s (not . p))


pair :: a -> a -> QSet a
pair x y = qset \p -> if p x then pure x else pure y

insert :: a -> QSet a -> QSet a
insert a s = point a <> s


contains :: (Eq a) => QSet a -> a -> Bool
contains s x = forsome s (== x)

member :: (Eq a) => a -> QSet a -> Bool
member = flip contains

----

peek s
  = find s \_ -> True

pop s = do
  x <- peek s
  pure (x, diff s $ point x)

setToList
  = unfoldr pop

setFromList
  = foldMap point


-- instance Foldable QSet where
--   foldr f e = foldr f e . setToList

----

bits = pair False True


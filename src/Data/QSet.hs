{-# LANGUAGE NoArrows #-}
module Data.QSet where

import Prelude hiding (filter, mapMaybe)

import Iso
import Data.Unfoldable
import Control.Search qualified as S
import Control.Search (SearchT)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Select

import Data.Functor.Apply
import Data.Functor.Bind as B
import Data.Witherable.Class
import Data.Group

import Algebra.Set
import Algebra.Lattice hiding (top)
import Algebra.Lattice qualified
import Algebra.PartialOrd
import Algebra.Heyting
import Data.Profunctor


newtype QSet r a
      = QSet { toSearch :: SearchT Maybe r a }
  deriving newtype
    ( Pointed
    , Functor, Apply, Applicative, Monad
    , Alternative, MonadPlus, MonadFail
    , Filterable, Semialign, Zip
    , Profunctor
    )
--   deriving
--     ( Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice
--     , Eq
--     , PartialOrd, Heyting
--     , Group
--     )
--     via WrappedSet (QSet a)


fromSearch :: SearchT Maybe r a -> QSet r a
fromSearch = coerce

qset :: ((a -> Maybe r) -> Maybe a) -> QSet r a
qset = QSet . S.searchT

find :: QSet r a -> (a -> Maybe r) -> Maybe a
find = S.runSearchT . toSearch

----

isoMUnitBool :: Maybe () <-> Bool
isoMUnitBool =
  Iso isJust \x -> () <$ guard x

isoQSetSearch :: QSet r a <-> SearchT Maybe r a
isoQSetSearch =
  Iso coerce coerce

toCont :: QSet r a -> ContT r Maybe a
toCont = toSearch >>> S.searchToContT

fromCont :: ContT a Maybe a -> QSet a a
fromCont = qset . runContT

----

instance Semigroup (QSet r a) where
  s <> t = B.join (pair s t)

instance Monoid (QSet r a) where
  mempty = qset $ const Nothing

instance Bind (QSet r) where
  join ss = qset \p ->
    flip find p -<< find ss \s -> forsome s p

instance Unfoldable (QSet r) where
  unfold = foldr insert mempty

-- instance Foldable (QSet r) where
--   foldr f e = foldr f e . setToList

instance (Show a, Eq a) => Show (QSet b a) where
  show = show . setToList

instance Eq a => Set (QSet b a)
  where
    bot
      = mempty

    union
      = (<>)

    intersect a b
      = filter (contains b) a

    -- subeq a b
    --   = forevery a $ contains b

    diff a b
      = filter (not . contains b) a

    top
      = undefined

----

forsome :: QSet r a -> (a -> Maybe r) -> Maybe r
forsome = runContT . toCont

forevery :: ∀ r a. QSet r a -> (a -> Maybe r) -> Bool
forevery s p = isNothing $ forsome s $ p >>> \case
  Just _  -> Nothing
  Nothing -> Just case () of {}

pair :: a -> a -> QSet r a
pair x y = qset \p -> pure if isJust $ p x then x else y

insert :: a -> QSet r a -> QSet r a
insert a s = point a <> s

query :: (a -> b -> Maybe r) -> a -> QSet r b -> Maybe r
query choose = flip forsome . choose

member :: Eq a => a -> QSet b a -> Bool
member s =
  isJust . query (\a b -> undefined <$ guard (a == b)) s

contains :: Eq a => QSet b a -> a -> Bool
contains = flip member

----

bool :: (a -> Bool) -> (a -> Maybe a)
bool f a = a <$ guard (f a)

peek s
  = find s \_ -> Just undefined

pop s = do
  x <- peek s
  pure (x, diff s $ point x)

setToList
  = unfoldr pop

setFromList :: [a] -> QSet r a
setFromList = unfold


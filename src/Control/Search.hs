{-# LANGUAGE NoArrows #-}
module Control.Search where

import Control.Monad.Trans.Select
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont

import Iso
import Data.Profunctor
import Data.Group
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Alt hiding (Alt)
import Data.Functor.Alt qualified as F
import Data.Witherable.Class


newtype SearchT m r a
      = SearchT { toSelectT :: SelectT r m a }
    deriving newtype
      ( Functor
      , MonadFail, MonadIO
      , MonadPlus
      )

----

type Search
   = SearchT Identity

fromSelectT :: SelectT m r a -> SearchT r m a
fromSelectT = coerce

searchT :: ((a -> m r) -> m a) -> SearchT m r a
searchT = coerce

search :: ((a -> r) -> a) -> Search r a
search = coerce

{-# INLINE runSearchT #-}
runSearchT :: SearchT m r a -> (a -> m r) -> m a
runSearchT = runSelectT .# toSelectT

runSearch :: Search r a -> (a -> r) -> a
runSearch = runSelect .# toSelectT

searchToContT :: Monad m => SearchT m r a -> ContT r m a
searchToContT = selectToContT .# toSelectT

searchToCont :: Monad m => Search r a -> Cont r a
searchToCont = searchToContT

mapSearchT :: (m a -> m a) -> SearchT m r a -> SearchT m r a
mapSearchT f = fromSelectT #. mapSelectT f .# toSelectT

mapSearch :: (a -> a) -> Search r a -> Search r a
mapSearch = mapSearchT .# (<#>)

----

isoSearchSelect :: SearchT r m a <-> SelectT m r a
isoSearchSelect = Iso coerce coerce

isoSearchStar :: SearchT m a b <-> Star m (Star m b a) b
isoSearchStar = Iso coerce coerce

isoSearchCostar :: SearchT m r a <-> Costar ((->) a) (m r) (m a)
isoSearchCostar = Iso coerce coerce

----

{-# INLINE findT #-}
findT = flip runSearchT
find  = flip runSearch

{-# INLINE edit #-}
edit f
  = searchT #. f .# runSearchT

{-# INLINE edit2 #-}
edit2 f a b
  = searchT #$ f (runSearchT a) (runSearchT b)

slift :: (m a -> m a) -> SearchT m r a -> SearchT m r a
slift f
  = edit \g a -> f (g a)

slift2 :: (m a -> m a -> m a) -> SearchT m r a -> SearchT m r a -> SearchT m r a
slift2 f =
  edit2 \g h a -> g a `f` h a

before :: (m a -> m b) -> SearchT m b c -> SearchT m a c
before ab = edit \bc ca -> bc (ab . ca)

----

member choose
  = flip forsomeT . choose

contains choose
  = flip (member choose)

subset choose s
  = foreveryT s . (contains choose)


forsome :: Search r a -> (a -> r) -> r
forsome s p
  = p $ find p s

forsomeT :: Bind m => SearchT m r a -> (a -> m r) -> m r
forsomeT s p
  = p -<< findT p s

forevery :: Group r => Search r a -> (a -> r) -> r
forevery s p
  = invert $ forsome s $ invert . p

foreveryT :: (Group (m r), Bind m) => SearchT m r a -> (a -> m r) -> m r
foreveryT s p
  = invert $ forsomeT s $ invert . p

----

type BP m = (Pointed m, Bind m)

deriving via WrappedApply (SearchT m r) instance
  Bind m => Semialign (SearchT m r)

deriving via WrappedApply (SearchT m r) instance
  Bind m => Zip (SearchT m r)

deriving via WrappedApply (SearchT m r) instance
  BP m => Applicative (SearchT m r)

instance Semigroup (m a) => Semigroup (SearchT m r a) where
  (<>) = slift2 (<>)

instance Monoid (m a) => Monoid (SearchT m r a) where
  mempty = searchT #$ const mempty

instance (Group (m a)) => Group (SearchT m r a) where
  invert = slift invert

instance F.Alt m => F.Alt (SearchT m r) where
  (<!>) = slift2 (<!>)

instance (BP m, Alternative m) => Alternative (SearchT m r) where
  empty = searchT #$ const empty
  (<|>) = slift2 (<|>)

instance BP m => Monad (SearchT m r) where
  (>>=) = (>>-)

instance Pointed m => Pointed (SearchT m r) where
  point = searchT #. const . point
  -- {-# INLINE point #-}

instance (BP m, Alternative m, F.Alt m) => Filterable (SearchT m r) where
  catMaybes s = s >>- maybe empty point
  filter  p s = [ a | a <- s, p a ]

instance Bind m => Apply (SearchT m r) where
  -- {-# INLINE (<.>) #-}
  (<.>) = edit2 \gf gx k ->
    let h f = f <$> gx (k . f)
     in h -<< gf ((>>- k) . h)

  -- {-# INLINE (.>) #-}
  m .> k = m >>- \_ -> k

instance Bind m => Bind (SearchT m r) where
  -- {-# INLINE (>>-) #-}
  g >>- f = searchT \k ->
    let h x = findT k (f x)
     in findT ((>>- k) . h) g >>- h

  -- {-# INLINE (join) #-}
  join = edit \s p ->
    findT p -<< s \s -> forsomeT s p

instance Functor m => Profunctor (SearchT m) where
  -- {-# INLINE lmap #-}
  lmap ab = edit
    \s -> s . (fmap ab .)

  -- {-# INLINE rmap #-}
  rmap f = edit
    \g -> fmap f . g . (. f)

  -- {-# INLINE dimap #-}
  dimap ab bc = edit
    \s ca -> fmap bc . s $ fmap ab . ca . bc


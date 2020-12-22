{-# LANGUAGE NoArrows #-}
module Data.Fold where

import Data.These
import Data.Bool
import Data.Foldable
import Data.Unfoldable
import Data.Profunctor
import Control.Arrow
import Iso
import Data.Bifunctor
import GHC.Exts (IsList (..))

import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Alt as F
import Data.Functor.Plus
import Data.Functor.Extend
import Data.Traversable.TreeLike
import Data.Witherable.Class as W


-- Hylomorphism. We are folding 'f' and unfolding 'g'
--
newtype F a b = F { runF :: ∀ f g. Folding f g => f a -> g b }


infixr 5 :>
pattern (:>) a as <- (unconsF->Just (a, as))
  where (:>) a as = consF a . as

pattern Nil <- (headF -> Nothing)
  where Nil = mempty


isoF :: Folding f f => (f a -> f b) <-> F a b
isoF = Iso (\a -> F $ refold a) runF
  where
    refold f = unfold . f . unfold

consF :: a -> F a a
consF = pro isoF . foldrCons

snocF :: a -> F a a
snocF = pro isoF . foldrSnoc

headF :: ∀ a b. F a b -> Maybe b
headF (F f) = f $ mempty @(Foldr a)

tailF :: F a a
tailF = pro isoF foldrTail

unconsF :: F a b -> Maybe (b, F a b)
unconsF t = (, tailF . t) <$> headF t

----

instance Unit        (F a b)  where unit      = Nil
instance Plus        (F a)    where zero      = Nil
instance MonadPlus   (F a)    where mzero     = Nil
instance MonadFail   (F a)    where fail _    = Nil
instance Alternative (F a)    where empty     = Nil; (<|>) = (<>)
instance F.Alt       (F a)    where (<!>)     = (<>)
instance Apply       (F a)    where (<.>)     = apDefault
instance Bind        (F a)    where join      = fold
instance Repeat      (F a)    where repeat    = ap (:>) repeat
instance ArrowZero    F       where zeroArrow = mempty
instance ArrowPlus    F       where (<+>)     = mplus
instance Monad       (F a)    where (>>=)     = (>>-)
instance Functor     (F a)    where fmap      = rmap
instance Unzip       (F a)    where unzip     = unzipDefault

deriving via WrappedPoint (F a)
  instance Applicative (F a)


instance Foldable (F x) where
  foldr f e (a:>as) = f a $ foldr f e as
  foldr f e _       = e

instance Unfoldable (F x) where
  unfold = foldr (:>) Nil

instance Profunctor F where
  rmap bc ab = arr bc . ab
  lmap ab bc = bc . arr ab

instance Traversable (F a) where
  traverse f = foldr cons_f (pure mempty)
    where cons_f x ys = liftA2 cons (f x) ys

instance Category F where
  id          = F unfold
  F bc . F ab = F (bc . ab)

instance Arrow F where
  arr =
    pro isoF . fmap @Foldr

  F l *** F r =
    pro isoF $ uncurry zip . bimap l r . unzip

instance Pointed (F a) where
  point a = consF a . mempty

instance Semigroup (F a b) where
  F l <> F r =
    pro isoF $ liftA2 (<>) l r

instance Monoid (F a b) where
  mempty = F \_ -> unfold $ mempty @(Foldr b)

instance Semialign (F a) where
  alignWith f (x:>xs) (y:>ys) = f (These x y) :> alignWith f xs ys
  alignWith f (x:>xs) _       = f (This  x  ) :> fmap (f . This) xs
  alignWith f _       (y:>ys) = f (That    y) :> fmap (f . That) ys
  alignWith f _       _       = Nil

instance Zip (F a) where
  zipWith f (x:>xs) (y:>ys) = f x y :> zipWith f xs ys
  zipWith _ _ _             = Nil

instance TreeLike (F a) where
  treeTraverse f g = \case
    a:>as -> liftA2 cons (f a) (g as)
    Nil   -> pure Nil

instance Filterable (F a) where
  filter    f t = pro isoF (W.filter    @[] f) . t
  mapMaybe  f t = pro isoF (W.mapMaybe  @[] f) . t
  catMaybes   t = pro isoF (W.catMaybes @[]  ) . t

instance Witherable (F a) where
  filterA p = go where
    go (x:>xs) = liftA2 (bool id (cons x)) (p x) (go xs)
    go _       = pure Nil

  {-# INLINE wither #-}
  wither f = foldr go (pure Nil) where
    go x r = liftA2 (maybe id cons) (f x) r

  {-# INLINE witherM #-}
  witherM f = foldr go (pure Nil) where
    go x r = f x >>=
      (\z -> case z of
        Nothing -> r
        Just y  -> cons y <$> r
      )

instance Extend (F a) where
  duplicated t@(_:>as) = t :> duplicated as
  duplicated        _  = Nil

----

instance IsList (F a b) where
  type Item (F a b) = b

  toList   = unfold
  fromList = unfold

instance Show b => Show (F a b) where
  show = show . unfold @[]

instance Eq b => Eq (F a b) where
  (==) l r = and $ alignWith go l r
    where
      go (These a b) = a == b
      go _           = False

instance Ord b => Ord (F a b) where
  compare l r = fold $ alignWith go l r
    where
      go (These a b) = compare a b
      go (This    _) = LT
      go _           = GT


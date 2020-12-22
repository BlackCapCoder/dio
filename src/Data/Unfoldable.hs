module Data.Unfoldable where

import Prelude hiding (head)
import Data.These
import Iso


-- unfold :: t a -> t a = id
class Unfoldable f where
  unfold :: Foldable g => g a -> f a

------

newtype Foldr a
      = Foldr { runFoldr :: ∀ b. (a -> b -> b) -> b -> b }
  deriving
    ( Functor
    )

instance Foldable Foldr where
  foldr f e (Foldr foldr) = foldr f e

instance Unfoldable Foldr where
  unfold t = Foldr \f z -> foldr f z t


refold f =
  unfold . f . unfold @Foldr


foldrCons :: a -> Foldr a -> Foldr a
foldrCons a (Foldr f) = Foldr
  \cons q -> cons a (f cons q)

foldrSnoc :: a -> Foldr a -> Foldr a
foldrSnoc a (Foldr f) = Foldr
  \cons q -> f cons (cons a q)

foldrTail :: Foldr a -> Foldr a
foldrTail (Foldr f) = Foldr \cons q ->
  f (\x rst g -> g x (rst cons)) (\_ -> q) \_ xs -> xs

------

type Folding i o
   = (Foldable i, Unfoldable o)

isoFolding :: (Folding f f, Folding g g) => f a <-> g a
isoFolding = Iso unfold unfold


head :: Foldable t => t a -> a
head = foldr1 \x _ -> x

tail :: Folding f g => f a -> g a
tail = refold foldrTail

cons :: Folding f g => a -> f a -> g a
cons = refold . foldrCons

snoc :: Folding f g => a -> f a -> g a
snoc = refold . foldrSnoc

uncons :: Folding f g => f a -> (a, g a)
uncons = liftA2 (,) head tail


-- safe head
head' :: (Foldable t, Alternative m) => t a -> m a
head' t | null t = empty | let = pure (head t)

-- safe uncons
uncons' :: (Folding f g, Alternative m) => f a -> m (a, g a)
uncons' t = (, tail t) <$> head' t


-- generic (:)
pattern (:-) :: (Folding f g, Folding g f) => a -> f a -> g a
pattern (:-) x xs <- (uncons'->Just (x,xs))
  where (:-) x xs = cons x xs

----

instance Semigroup (Foldr a) where
  Foldr a <> Foldr b =
    Foldr $ liftA2 (.) a b

instance Monoid (Foldr a) where
  mempty = Foldr \_ a -> a

instance Semialign Foldr where
  alignWith f (x:-xs) (y:-ys) = f (These x y) :- alignWith f xs ys
  alignWith f (x:-xs) _       = f (This  x  ) :- fmap (f . This) xs
  alignWith f _       (y:-ys) = f (That    y) :- fmap (f . That) ys
  alignWith f _       _       = mempty

instance Zip Foldr where
  zipWith f (x:-xs) (y:-ys) = f x y :- zipWith f xs ys
  zipWith _ _ _             = mempty

instance Unzip Foldr where
  unzip = unzipDefault

-----

instance Unfoldable [] where
  unfold = foldr (:) []

instance Unfoldable Maybe where
  unfold = foldr (const.pure) empty


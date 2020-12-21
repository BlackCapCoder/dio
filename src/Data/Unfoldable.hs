module Data.Unfoldable where

import Prelude hiding (head)


-- unfold :: t a -> t a = id
class Unfoldable f where
  unfold :: Foldable g => g a -> f a

instance Unfoldable [] where
  unfold = foldr (:) []

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

-----

pattern Nil <- (head' @Foldr -> Nothing)
  where Nil = Foldr \_ a -> a

instance Traversable Foldr where
  traverse f = foldr cons_f (pure Nil)
    where cons_f x ys = liftA2 cons (f x) ys

traverseDef :: (Applicative f, Folding t t) => (a -> f b) -> t a -> f (t b)
traverseDef f = fmap unfold . traverse f . unfold @Foldr


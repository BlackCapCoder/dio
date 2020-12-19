module ListZipper where

data Lz a
   = Lz' [a] [a]
   deriving Show

instance Copointed Lz where
  copoint (Lz _ x _) = x

modify f = \case
  Lz l x r -> Lz l (f x) r

pattern Lz ls r rs =
  Lz' ls (r : rs)

right = \case
  Lz  ls x (r:rs) -> Lz (x:ls) r rs
  Lz' ls rs       -> Lz' rs (reverse ls)

left = \case
  Lz' (l:ls) rs         -> Lz ls l rs
  Lz' _ (reverse->r:rs) -> Lz rs r []


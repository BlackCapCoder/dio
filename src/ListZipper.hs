module ListZipper where


-- A finitary list zipper
data Lz a
   = Lz' [a] [a]
 deriving
   ( Show, Eq
   , Functor, Foldable, Traversable
   )

-- Constructor with explicit focus
pattern Lz ls r rs =
  Lz' ls (r : rs)

-- Get focus. Throws if empty
instance Copointed Lz where
  copoint (Lz _ x _) = x

-- Modify focus
modify f = \case
  Lz l x r -> Lz l (f x) r

-- Shift left
left = \case
  Lz' (l:ls) rs         -> Lz ls l rs
  Lz' _ (reverse->r:rs) -> Lz rs r []

-- Shift right
right = \case
  Lz  ls x (r:rs) -> Lz (x:ls) r rs
  Lz' ls rs       -> Lz' [] (reverse ls <> rs)


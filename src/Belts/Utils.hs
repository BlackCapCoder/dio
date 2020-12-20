module Belts.Utils where

import Belts.Samplable


-- Act like 's', but with a time offset
data TimeOffset s
   = TimeOffset Time s

instance Samplable s => Samplable (TimeOffset s)
  where
    sample l i t (TimeOffset off s)
      = sample l i (t+off) s

----

-- Act like 's1' before the given time, then act like 's2'
data TimeSwitch s1 s2
   = TimeSwitch Time s1 s2

instance (Samplable s1, Samplable s2) => Samplable (TimeSwitch s1 s2)
  where
    sample l i t (TimeSwitch x a b)
      | t < x = sample l i t a
      | let   = sample l i t b

----

-- The empty and full belts
data Empty = Empty
data Full  = Full

instance Samplable Empty where
  sample _ _ _ _ = False

instance Samplable Full where
  sample _ _ _ _ = True

----

-- Act like 's1' up until the given index, then 's2'
data Conc s1 s2
   = Conc Index s1 s2

instance (Samplable s1, Samplable s2) => Samplable (Conc s1 s2)
  where
    sample l i t (Conc x s1 s2)
      | i < x = sample    x   i      t s1
      | let   = sample (l-x) (i - x) t s2


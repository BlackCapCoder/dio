{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
--
-- Code from an old project.
--
module Belts.Messy where

import Numeric.Natural
import Debug.Trace
import Data.List


infixl 5 %
(%) = mod

-- type N    = Natural
type N    = Integer
type Time = N
type Link = N

class Sample t where

  -- Number of links
  len    :: t -> N

  -- Whether items can disappear off of the right edge
  spill :: t -> Bool

  -- Modify length and sink
  change :: N -> Bool -> t -> t

  -- Given that the system has never stalled, how many
  -- unique items has been processed?
  potential :: Time -> t -> N

  -- Whether there is an item on the given index, at the given time
  sample :: Link -> Time -> t -> Bool
  sample l t s
    = count t l 1 s /= 0

  -- Number of consecutive items, counting from right to left
  back :: Time -> t -> N
  back = backDefault

  -- Number of consecutive non-items, counting from the left
  off :: Time -> t -> N
  off = offDefault

  -- Total number of items
  num :: Time -> t -> N
  num t s = countFrom t 0 s

  -- Total number of items from and including the given link
  countFrom :: Time -> Link -> t -> N
  countFrom = countFromDefault

  -- count t i n s: Total number of items between (and including) 'i' and 'i+n'
  count :: Time -> Link -> N -> t -> N
  count t i n s
    = countFrom t i s - countFrom t (i+n) s


sampleDefault :: Sample t => Link -> Time -> t -> Bool
sampleDefault l t s
  = count t l 1 s /= 0

offDefault :: Sample t => Time -> t -> N
offDefault t s
  = until (\i -> i >= len s || sample i t s) (+1) 0

backDefault :: Sample t => Time -> t -> N
backDefault t s
  = until (\i -> i >= len s || sample (len s - 1 - i) t s == False) (+1) 0

countFromDefault :: Sample t => Time -> Link -> t -> N
countFromDefault t i s
  = fi . length
  $ filter (\j -> sample j t s) [i..len s - 1]


-------

render :: Sample s => Time -> s -> String
render t s
  = [ if sample i t s then '*' else '-'
    | i <- [0..len s-1]
    ]

renders :: Sample s => Time -> N -> s -> String
renders t n s
  = unlines [ render x s <> "  " <> show x
            | i <- [0..pred n]
            , let x = t + i
            ]

test n = putStr . renders 0 n

-------

data Simple = Simple
  { links  :: N
  , source :: N
  , sink   :: Bool
  }
  deriving
    ( Eq, Ord, Show
    )


timeToFill :: N -> Simple -> Time
timeToFill n Simple{..}
  = links + n*source - n - source

instance Sample Simple
  where

    len
      = links

    spill
      = sink

    change l s x
      = x { links = l, sink = s }

    potential t Simple{..}
      = 1 + div t source

    sample :: Link -> Time -> Simple -> Bool
    sample l t s@Simple{..}
      | source <= 0 = False
      | l > t       = False
      | not sink
      , t >= timeToFill (links - l) s = True
      | otherwise = l % source == t % source

    back (succ->t) Simple{..}
      | source <= 0 = 0
      | t < links   = 0
      | source == 1 = links
      | sink
      = if | mod t source == 0 -> 1
           | otherwise         -> 0
      | otherwise = min links $ 1 + div (t-links) (source-1)

    off t s@Simple{..}
      = min (t % source) $ links - back t s

    num t s@Simple{..}
      | source <= 0 = 0
      | t < links   = 1 + div t source
      | sink        = div (links + source - mod t source - 1) source
      | let         = min links (1 + div t source)

    countFrom t i s@Simple{..}
      | source <= 0 = 0
      | t < i       = 0
      | t < links   = 1 + div (t-i) source
      | sink        = div (l + source - mod (t-i) source - 1) source
      | let         = min l (1 + div (t-i) source)
      where
        l = links - i



instance Semigroup Simple
  where

  Simple l1 s1 b1 <> Simple l2 s2 b2
    = Simple
    { links  = l1 + l2
    , source = s1
    , sink   = b2
    }

-----------

-- 'Cork s' is like 's', except that no new items are allowed
-- in from the left, starting with and following the given time.

data Cork s = Cork s Time

-- A time when the system has settled
settled (Cork s j)
  = ( j+o -- Time settled
    , b2  -- items backed up at this time
    )
  where
    n = num j s
    o = len s - n - off (j-1) s
    b1 | spill s = 0 | let = n
    b2 | spill s = 0 | let = back (j+o) s

instance Sample s => Sample (Cork s)
  where

  len (Cork s _)
    = len s

  spill (Cork s _)
    = spill s

  change l s (Cork x t)
    = Cork (change l s x) t

  potential t (Cork s j)
    = potential (min t (j-1)) s

  sample i t c@(Cork s j)
    | t < j     = sample i t s
    | i > t - j = sample i t s
    | t >= k
    , i >= len s - b
    = True
    | otherwise = False
    where
      (k, b) = settled c

  back t c@(Cork s j)
    | t < j || t < k = back t s
    | let = b
    where
      (k, b) = settled c

  off t c@(Cork s j)
    | t < j = off t s
    | t < k = len s - b - k + t + 1
    | let   = len s - b
    where
      (k, b) = settled c

  num t c@(Cork s j)
    | t < j          = num t s
    | not (spill s)  = n
    | t >= j + l - o = 0 -- no items left
    | let = countFrom t i s
    where
      n = num (j-1) s
      o = off (j-1) s + 1
      l = len s
      i = o+t-j

  countFrom t i c@(Cork s j)
    | t <  j = countFrom t i s
    | t >= k
    = if | i <= l-b -> b
         | let      -> b - (i - (l-b))
    | let = countFrom t (max i $ t-j+1) s
    where
      (k, b) = settled c
      l = len s

----------

-- At the given time, 'Conc a b' will connect the rightmost
-- link of 'a' to the leftmost link of 'b'.
--
-- Prior to this 'a' and 'b' act independently.

data Conc a b = Conc a b Time

instance (Sample a, Sample b) => Sample (Conc a b)
  where

  len (Conc a b _)
    = len a + len b

  spill (Conc _ b _)
    = spill b

  change l s (Conc a b j)
    | l > len a = Conc a (change (l - len a) s b) j
    | let = error "TODO"

  sample i t (Conc a b j)
    | t < j
    = if | i < len a -> sample i t a
         | otherwise -> sample (i - len a) t b

    | t < j2
    , i > len a + t - j - b1
    , i < len a + t - j + 1
    = True

    | t > j2
    , i - t + j2 >= len a + len b - b2
    = True

    | t < j2
    , i >= len a + t - j
    = sample (i - len a) t b'

    | otherwise
    = sample i t a'

    where
      a' = change (len a + len b - bset) (spill b) a
      b' = Cork b j
      (tset, bset) = settled b'
      b1 = back (j-1) a
      b2 = b1 + bset
      j2 = j + len b - bset - 1 -- left backlog hits right backlog

  back t (Conc a b j)
    | t < j2
    = if | back t b < len b -> back t b
         | let              -> back t b + back t a
    | let = back t a' + bset
    where
      b' = Cork b j
      (tset, bset) = settled b'
      j2 = j + len b - bset - 1 -- left backlog hits right backlog
      a' = change (len a + len b - bset) (spill b) a

  off t (Conc a b j)
    | t < j = off t a
    | let   = off t a'
    where
      a' = change (len a + len b - bset) (spill b) a
      b' = Cork b j
      (tset, bset) = settled b'

  num t (Conc a b j)
    | t < j = num t a  + num t b
    | let   = num t a' + num t b'
    where
      a' = change (len a + len b - bset) (spill b) a
      b' = Cork b j
      (tset, bset) = settled b'

  countFrom t i (Conc a b j)
    | t < j
    = if | i < len a -> countFrom t i a + num t b
         | let       -> countFrom t (i - len a) b

    | t < j2, i > len a + t - j
    = countFrom t (i - len a) b'

    | t < j2, i > len a - b2 + t - j + 1
    = countFrom t (i - len a) b' + i - (len a - b2 + t - j + 1)

    | i < len a' = countFrom t i a' + bset
    | let        = bset + len a' - i
    where
      a' = change (len a + len b - bset) (spill b) a
      b' = Cork b j
      (tset, bset) = settled b'
      j2 = j + len b - bset - 1 -- left backlog hits right backlog
      b2 = back j2 a'


--------

data LeftHalf  a = LeftHalf  N Time a
data RightHalf a = RightHalf N Time a

-- At time 't', cut the given belt into 2 halves at index 'n'
cut n t = liftA2 (,) (LeftHalf n t) (RightHalf n t)


instance Sample a => Sample (LeftHalf  a)
  where

  len   (LeftHalf c _ _) = c
  spill _                = False

  change l _ (LeftHalf c j a)
    = LeftHalf l j a

  sample i t h@(LeftHalf c j a)
    | t < j = sample i t a
    | not (spill a)
    = sample i t $ change (l + nr) False a
    | let
    = sample i t $ change (l + p - nl) False a
    where
      l  = len h
      nr = countFrom (j-1) c a
      n  = num (j-1) a
      nl = n - nr
      p  = potential (j-1) a


instance Sample a => Sample (RightHalf a) where

  len   (RightHalf c _ a) = len a - c
  spill (RightHalf _ _ a) = spill a

  change l x (RightHalf c j a)
    = RightHalf c j $ change (l + c) x a

  sample i t h@(RightHalf c j a)
    | i > t-j = sample (i+c) t a
    | spill h = False
    | let     = i >= l - n
    where
      l = len h
      n = countFrom (j-1) c a


--------

ex1 = Simple 6 5 False
ex2 = Simple 11 4 False
ex3 = Conc ex1 ex2 11
ex4 = Conc ex3 ex3 17
ex5 = Conc ex4 ex4 25
ex6 = Conc ex5 ex5 35
exc = Cork (Simple 8 3 False) 11
ex7 = let (l,r) = cut 4 8 ex2 in Conc l (Conc ex1 r 16) 16
ex8 = Conc ex7 (Simple 20 0 False) 26

long = Simple 20 4 False

--------

data Belt where
  Belt :: Sample s => Time -> s -> Belt

instance Sample Belt where
  len           (Belt _ s) = len s
  spill         (Belt _ s) = spill s
  potential t   (Belt q s) = potential (t+q) s
  sample a b    (Belt q s) = sample a (b+q) s
  back a        (Belt q s) = back (a+q) s
  off a         (Belt q s) = off (a+q) s
  num a         (Belt q s) = num (a+q) s
  countFrom a b (Belt q s) = countFrom (a+q) b s
  count a b c   (Belt q s) = count (a+q) b c s
  change n b    (Belt t s) = Belt t (change n b s)

belt :: N -> N -> Bool -> Belt
belt len intv sink = Belt 0 (Simple len intv sink)

chop :: N -> Belt -> (Belt, Belt)
chop i (Belt t s)
  | (l,r) <- cut i t s = (Belt t l, Belt t r)

conc (Belt tl sl, Belt tr sr)
  | t <- max tl tr
  = Belt t (Conc sl sr t)

instance Semigroup Belt where
  (<>) = curry conc

b1 = belt 10 3 False
b2 = belt 8  5 True

wait n (Belt t s)
  = Belt (t+n) s

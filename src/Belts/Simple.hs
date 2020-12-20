module Belts.Simple where

import Belts.Samplable


-- A continuous stretch of belts flowing left to right.
--
-- New items appear on the leftmost belt at a uniform rate `source`.
-- For every unit of time, all items are moved one belt rightwards.
--
-- If `sink` is true, items are moved out of the system by
-- the rightmost belt.
--
-- If `sink` is false, items can never leave the system and
-- will clog from right to left.
--
data Simple = Simple
  { source :: Interval
  , sink   :: Bool
  }
  deriving
    ( Eq, Ord, Show
    )

-- How long to clog N belts
timeToFill :: Length -> Count -> Interval -> Time
timeToFill l n source
  = l + n*source - n - source


instance Samplable Simple
  where
    sample :: Length -> Index -> Time -> Simple -> Bool
    sample len l t s@Simple{..}
      | source <= 0 = False
      | l > t       = False
      | not sink
      , t >= timeToFill len (len - l) source = True
      | otherwise = l % source == t % source

----

ex1 = Simple 4 False

{- > timeToFill 10 10 4
   36

   > test 10 (36+1) ex1

   *---------  0
   -*--------  1
   --*-------  2
   ---*------  3
   *---*-----  4
   -*---*----  5
   --*---*---  6
   ---*---*--  7
   *---*---*-  8
   -*---*---*  9
   --*---*--*  10
   ---*---*-*  11
   *---*---**  12
   -*---*--**  13
   --*---*-**  14
   ---*---***  15
   *---*--***  16
   -*---*-***  17
   --*---****  18
   ---*--****  19
   *---*-****  20
   -*---*****  21
   --*--*****  22
   ---*-*****  23
   *---******  24
   -*--******  25
   --*-******  26
   ---*******  27
   *--*******  28
   -*-*******  29
   --********  30
   --********  31
   *-********  32
   -*********  33
   -*********  34
   -*********  35
   **********  36
-}


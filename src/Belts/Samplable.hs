module Belts.Samplable
  ( module Belts.Samplable
  )
  where


infixl 5 %
(%) = mod

type N        = Integer
type Time     = N
type Index    = N
type Length   = N
type Count    = N
type Interval = N


class Samplable s
  where
    -- Whether there is an item on the given index, at the given time
    sample :: Length -> Index -> Time -> s -> Bool


-----

render :: Samplable s => Length -> Time -> s -> String
render l t s
  = [ if sample l i t s then '*' else '-'
    | i <- [0..l-1]
    ]

renders :: Samplable s => Length -> Time -> Count -> s -> String
renders l t n s
  = unlines [ render l x s <> "  " <> show x
            | i <- [0..pred n]
            , let x = t + i
            ]

test l n = putStr . renders l 0 n


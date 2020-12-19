module Dio where

import Data.Euclidean
import Data.Semigroup
import Iso


-- A linear diophantine equation 'ax + by = c'
-- where 'x' and 'y' are unknown
data Linear n = Lin { a,b,c :: n }
  deriving
    ( Show, Eq, Ord, Functor
    )

-- Given any arbitrary integer it produces
-- an unique solution (x,y)
type Solution n
   = Maybe (n -> (n, n))


solveLinear :: _ => Linear n -> Solution n
solveLinear Lin {..}
  = [ \k -> (x + k*v, y - k*u) | q == 0 ]
  where
    (d, e) = gcdExt a b
    (h, q) = divMod c d
    f      = div (a*e-d) (-b)
    (x, y) = (e*h, f*h)
    (u, v) = (quot a d, quot b d)

-- Solve and pass input
runLinear l n =
  ($ n) <$> solveLinear l


------

{-
   You are standing on a pogo stick at the surface of a circle of
   circumferance C. A distance D away from you is a piece of candy.
   Your pogo stick can only jump jumps of some integer length L.
   How many hops H does it take to land on the candy?

   Examples:
     (C=10, D=1, L=2) -> no solution
     (C=23, D=3, L=5) -> 19 jumps
-}

data Pogo i
   = Pogo { l :: i   -- Jump length
          , d :: i   -- Initial distance to the candy
          , c :: i } -- Circle circumference
   deriving
     ( Show, Eq, Ord, Functor
     )


-- How many jumps to get back to the starting point?
roundtrip Pogo {..} =
  div c $ gcd c l

-- H = Number of hops to land on the candy
getH Pogo {..} t =
  div (d + t*c) l

-- T = Number of times passed all the way around the circle
getT Pogo {..} h =
  div (h*l - d) c

-- D = H*L (mod C)
-- D = H*L - T*C


-- Linear: ax + by = c
-- Pogo:   HL - TC = D
isoPogoLin :: Pogo n <-> Linear n
isoPogoLin =
  Iso do \Pogo {l=a, c=b, d=c} -> Lin  {..}
      do \Lin  {a=l, b=c, c=d} -> Pogo {..}

-- Solve for H
solvePogo Pogo {..}
  | g == 1       = Just $ mod x c
  | mod d g /= 0 = Nothing
  | otherwise    = solvePogo $ Pogo (div c g) (div l g) (div d g)
  where
    (g,h) = gcdExt l c
    x     = d*h + c*div (d*h) (-c)


-- When will we first land on the
-- cell at index 'i'?
--
-- Returns a new Pogo such that
-- solving for H gives the answer
--
cell Pogo {..} i
   = Pogo { d = mod (i - d) c, .. }

-- Number of times we have landed on
-- cell 'i' after 't' time
--
countCell p@Pogo {..} i t
  =
    fromMaybe 0
 do
    n <- solvePogo $ cell p i

    let
      v = gcd c l
      g = div q c
      q = t*v
      h = v*n + c + c*g

    -- floor $ g + c/h + q/h
    pure $ div (g*h + c + q) h

----

-- A small fucktoid tarpit
data FOp
   = Jmp Int
   | Jz  Program
   | Neg

type Program
   = [FOp]

-- List zipper
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


type Bit  = Bool
type Tape = Lz Bit

blankTape :: Int -> Tape
blankTape s = Lz' [] $ replicate s False

runFuck :: Tape -> Program -> Tape =
  foldr \case
    Neg   -> modify not
    Jz  x -> until copoint (`runFuck` x)
    Jmp n
      | n < 0 -> abs n `times` left
      | let   ->     n `times` right
  where
    times n =
      appEndo .# stimes n #. Endo

runFuck' =
  runFuck $ blankTape 30_000

----

{-
  Pogo can emulate fucktoid loops.

  Boolfuck code:

    [ >>> ! >> ]

  Equivalient FOp:

    Jz [Jmp 3, Neg, Jmp 2]

  Equivalient Pogo:

    Pogo { l = 5 -- number of > (minus number of <)
         , d = 3 -- index  of ! }

  Under this interpretation of Pogo ..

    C = length of the memory tape
    L = total distance jumped in a single iteration
    D = distance jumped before the Neg instruction
    H = number of iterations before halting
-}


-- The loop diverges iff False
halts =
  isJust . solvePogo

-- The state of cell I at time T
-- assuming it was False initially
cellState p i t
  = odd $ countCell p i t


-- Pogo where we only care about termination
data Termination i (c :: Nat)
   = Term { l, d :: i }

termToPogo t@Term {..}
  = Pogo { c = fi $ natVal t, .. }


-- The result has the same solution H as a Pogo
-- having both candies Da and Db.
--
-- In fucktoid terms: a loop having both
-- Neg instructions.
--
-- `cellState` cannot recover the memory tape
-- of composed Pogos, thus the Term wrapper.
--
instance
  ( Ring i, Euclidean i, Integral i, KnownNat c
  ) => Semigroup (Termination i c)

  where

    Term la da <> Term lb db =

      let f  = solvePogo . termToPogo
          tl = Term (la+lb) da :: Termination i c
          tr = Term (la+lb) db :: Termination i c

      in case (f tl, f tr) of
        (Nothing, _)             -> tr
        (_, Nothing)             -> tl
        (Just a, Just b) | a < b -> tl
                         | let   -> tr


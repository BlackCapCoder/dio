module Data.DistinctFactors
  ( DistinctFactors ()
  , getDistinct
  , fromPrime
  , unsafeDistinct
  , distinct
  )
  where

import Math.NumberTheory.Primes


-- A number known to not to have any
-- duplicates in its prime factorisation
--
newtype DistinctFactors a
      = DistinctFactors { getDistinct :: a }
  deriving
    ( Show, Eq, Ord
    )

unsafeDistinct :: a -> DistinctFactors a
unsafeDistinct = coerce

distinct :: UniqueFactorisation a => a -> DistinctFactors a
distinct
  = coerce
  . factorBack
  . fmap (\(a,_) -> (a,1))
  . factorise

fromPrime :: Prime a -> DistinctFactors a
fromPrime = coerce . unPrime


module Bits.Misc where

import Control.Exception (AssertionFailed (..), assert, try)
import Data.List (unfoldr)

chunks :: Int -> [a] -> [[a]]
chunks n = unfoldr f
  where
    f [] = Nothing
    f xs = Just (splitAt n xs)

-- | Quicksort (after Hudak)
qs :: (Ord t) => [t] -> [t]
qs [] = []
qs (x : xs) =
  qs [y | y <- xs, y < x]
    ++ [x]
    ++ qs [y | y <- xs, y >= x]

-- | Sieve of... (Erasothenes ?)
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes :: [Int]
primes = sieve [2 ..]

-- | Useful canary
-- https://stackoverflow.com/questions/52227157/how-is-assert-used
assertionCanary :: IO Bool
assertionCanary = do
  assertionsWorking <- try $ assert False $ return ()
  return $
    case assertionsWorking of
      Left (AssertionFailed _) -> True
      _ -> False

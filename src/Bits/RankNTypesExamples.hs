{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Bits.RankNTypesExamples
-- Description : Exploring RankNTypes
--
-- Code from:
-- <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
module Bits.RankNTypesExamples where

import Control.Monad (replicateM)
import Control.Monad.State
import Data.Char
import Data.Kind (Type)
import System.Random

-- | A monomorphic function
--
-- >>> intId 42
-- 42
intId :: Integer -> Integer
intId x = x

-- | A second example
--
-- >>> doubleId 42.42
-- 42.42
doubleId :: Double -> Double
doubleId x = x

-- * Rank-1 Polymorphism

-- | A Rank-1 polymorphic function
--
-- The type is universally quanitified over 'a'
--
-- >>> identity 42
-- 42
-- >>> identity "forty-two"
-- "forty-two"
identity :: forall a. a -> a
identity x = x

-- * Rank-2 and higher polymorphism

type IdFunc = forall a. a -> a

identity' :: IdFunc
identity' x = x

-- | 'someInt' is Rank-2 polymorphic: the 'forall' is on the left of the
-- function arrow
--
-- >>> someInt identity
-- 3
someInt :: IdFunc -> Integer
someInt f = f 3

type SomeInt = IdFunc -> Integer

-- | 'someOtherInt' is Rank-3 polymorphic, because the quantifier is in the third
-- level of necessary parentheses:
--
-- >>> someOtherInt someInt
-- 6
someOtherInt :: SomeInt -> Integer
someOtherInt si = si id + si id

-- * Example: Random Numbers

data Player = Player
  { playerName :: String,
    playerPos :: (Double, Double)
  }
  deriving (Show)

type GenAction m = forall a. (Random a) => m a

type GenActionR m = forall a. (Random a) => (a, a) -> m a

genRandom :: (RandomGen g) => GenAction (State g)
genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
  liftIO (putStrLn "Generating random player...")
  len <- genR (8, 12)
  name <- replicateM len (genR ('a', 'z'))
  x <- genR (-100, 100)
  y <- genR (-100, 100)
  liftIO (putStrLn "Done.")
  return (Player name (x, y))

main :: IO ()
main = randomPlayer randomRIO >>= print

-- * Scott Encoding

data List a
  = Cons a (List a)
  | Nil
  deriving (Eq, Show)

uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co _ (Cons x xs) = co x xs
uncons _ ni Nil = ni

listNull :: List a -> Bool
listNull = uncons (\_ _ -> False) True

listMap :: (a -> b) -> List a -> List b
listMap f = uncons (\x xs -> Cons (f x) (listMap f xs)) Nil

-- | Scott-encoded lists are defined in terms of what happens when we 'uncons' them
newtype ListS a = ListS {unconsS :: forall r. (a -> ListS a -> r) -> r -> r}

instance (Show a) => Show (ListS a) where
  show _ = "<ListS>"

nilS :: ListS a
nilS = ListS (\_ ni -> ni)

consS :: a -> ListS a -> ListS a
consS x xs = ListS (\co _ -> co x xs)

class Listable t where
  toList :: t a -> [a]

instance Listable ListS where
  -- \| 'toList' converts a Scott-encoded list into a normal list
  --
  -- >>> let sl = consS 3 (consS 2 (consS 1 nilS))
  -- >>> toList sl
  -- [3,2,1]
  toList (ListS f) = f (\x xs -> x : toList xs) []

instance Functor ListS where
  -- >>> let sl = consS 3 (consS 2 (consS 1 nilS))
  -- >>> toList $ fmap (*3) sl
  -- [9,6,3]
  fmap f (ListS g) = g (\x xs -> consS (f x) (fmap f xs)) nilS

-- * Church Encoding

-- | Church-encoded lists are defined in terms of what happens when we fold them
newtype ListC a = ListC {foldC :: forall r. (a -> r -> r) -> r -> r}

instance (Show a) => Show (ListC a) where
  show _ = "<ListC>"

nilC :: ListC a
nilC = ListC (\_ z -> z)

consC :: a -> ListC a -> ListC a
consC x (ListC xs) = ListC (\f z -> f x (xs f z))

instance Listable ListC where
  -- \| 'toList' converts a Church-encoded list into a normal list
  --
  -- >>> let cl = consC 3 (consC 2 (consC 1 nilS))
  -- >>> toList cl
  -- [3,2,1]
  toList (ListC f) = f (:) []

instance Functor ListC where
  -- >>> let cl = consC 3 (consC 2 (consC 1 nilS))
  -- >>> toList $ fmap (*3) cl
  -- [9,6,3]
  fmap f (ListC g) = g (\x xs -> consC (f x) xs) nilC

-- * GADTs and continuation passing style

data Some :: Type -> Type where
  SomeInt :: Int -> Some Int
  SomeChar :: Char -> Some Char
  Anything :: a -> Some a

unSome :: Some a -> a
unSome (SomeInt x) = x + 3
unSome (SomeChar c) = toLower c
unSome (Anything x) = x

-- | 'SomeC' is the CPS-encoded version of 'Some'
newtype SomeC a = SomeC
  { runSomeC ::
      forall r.
      ((a ~ Int) => Int -> r) ->
      ((a ~ Char) => Char -> r) ->
      (a -> r) ->
      r
  }

-- | 'unSomeC'
--
-- >>> let a = SomeC (\f _ _ -> f 4)
-- >>> let b = SomeC (\_ g _ -> g 'A')
-- >>> let c = SomeC (\_ _ h -> h "Hello!")
-- >>> unSomeC a
-- 7
-- >>> unSomeC b
-- 'a'
-- >>> unSomeC c
-- "Hello!"
unSomeC :: SomeC a -> a
unSomeC a = runSomeC a (\x -> x + 3) (\c -> toLower c) (\x -> x)

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Bits.IteratorEssence where

import Data.Bifunctor
import Prelude hiding (drop, map, take)
import qualified Prelude

-- * Origami Programming

newtype Fix s a = In {out :: s a (Fix s a)}

deriving instance (Eq (s a (Fix s a))) => Eq (Fix s a)

deriving instance (Ord (s a (Fix s a))) => Ord (Fix s a)

deriving instance (Show (s a (Fix s a))) => Show (Fix s a)

map :: (Bifunctor s) => (a -> b) -> Fix s a -> Fix s b
map f = In . bimap f (map f) . out

fold :: (Bifunctor s) => (s a b -> b) -> Fix s a -> b
fold f = f . bimap id (fold f) . out

unfold :: (Bifunctor s) => (b -> s a b) -> b -> Fix s a
unfold f = In . bimap id (unfold f) . f

-- ** List

data ListF a b
  = NilF
  | ConsF a b
  deriving (Eq, Ord, Show, Functor)

type List a = Fix ListF a

instance Bifunctor ListF where
  bimap _ _ NilF = NilF
  bimap f g (ConsF x y) = ConsF (f x) (g y)

nil :: List a
nil = In NilF

cons :: a -> List a -> List a
cons x y = In (ConsF x y)

-- $
--
-- >>> let xs = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))
-- >>> map (*3) xs
-- In {out = ConsF 3 (In {out = ConsF 6 (In {out = ConsF 9 (In {out = ConsF 12 (In {out = ConsF 15 (In {out = NilF})})})})})}

-- ** Tree

data TreeF a b
  = EmptyF
  | NodeF a b b
  deriving (Eq, Ord, Show, Functor)

type Tree a = Fix TreeF a

instance Bifunctor TreeF where
  bimap _ _ EmptyF = EmptyF
  bimap f g (NodeF x y z) = NodeF (f x) (g y) (g z)

empty :: Tree a
empty = In EmptyF

node :: a -> Tree a -> Tree a -> Tree a
node x y z = In (NodeF x y z)

-- |
--
-- >>> let t = node 4 (node 2 (node 1 empty empty) (node 3 empty empty)) (node 6 (node 5 empty empty) (node 7 empty empty))
-- >>> fold treeSum t
-- 28
treeSum :: (Num a) => TreeF a a -> a
treeSum EmptyF = 0
treeSum (NodeF x y z) = x + y + z

-- ** BTree

data BTreeF a b
  = TipF a
  | BinF b b
  deriving (Eq, Ord, Show, Functor)

type BTree a = Fix BTreeF a

instance Bifunctor BTreeF where
  bimap f _ (TipF x) = TipF (f x)
  bimap _ g (BinF y z) = BinF (g y) (g z)

tip :: a -> BTree a
tip = In . TipF

bin :: BTree a -> BTree a -> BTree a
bin y z = In (BinF y z)

-- |
--
-- >>> unfold insert [1,2,3]
-- In {out = BinF (In {out = TipF 1}) (In {out = BinF (In {out = TipF 2}) (In {out = TipF 3})})}
insert :: (Num a) => [a] -> BTreeF a [a]
insert [] = error "this shouldn't happen"
insert [a] = TipF a
insert xs = BinF left right
  where
    len = length xs
    split = len `div` 2
    remainder = len `mod` 2
    left = Prelude.take split xs
    right = Prelude.take (split + remainder) (Prelude.drop split xs)

data Stream a = SCons a (Stream a)
  deriving (Eq, Show)

instance Functor Stream where
  fmap f (SCons x xs) = SCons (f x) (fmap f xs)

instance Applicative Stream where
  pure x = xs where xs = SCons x xs
  (SCons f fs) <*> (SCons x xs) = SCons (f x) (fs <*> xs)

drop :: (Eq a, Num a) => a -> Stream b -> Stream b
drop 0 (SCons _ xs) = xs
drop n (SCons _ xs) = drop (n - 1) xs

take :: (Eq a, Num a) => a -> Stream b -> [b]
take 0 _ = []
take n (SCons x xs) = x : take (n - 1) xs

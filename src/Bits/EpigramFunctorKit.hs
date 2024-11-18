{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bits.EpigramFunctorKit where

import Data.Traversable (foldMapDefault)

newtype I x = I {unI :: x} deriving (Show)

newtype K a x = K {unK :: a} deriving (Show)

data (p :+: q) x = L (p x) | R (q x) deriving (Show)

data (p :*: q) x = p x :*: q x deriving (Show)

instance Functor I where
  fmap f (I x) = I (f x)

instance Functor (K a) where
  fmap _ (K a) = K a

instance (Functor p, Functor q) => Functor (p :+: q) where
  fmap f (L px) = L (fmap f px)
  fmap f (R qx) = R (fmap f qx)

instance (Functor p, Functor q) => Functor (p :*: q) where
  fmap f (px :*: qx) = fmap f px :*: fmap f qx

instance Foldable I where
  foldMap = foldMapDefault

instance Foldable (K a) where
  foldMap = foldMapDefault

instance (Traversable p, Traversable q, Foldable p, Foldable q) => Foldable (p :+: q) where
  foldMap = foldMapDefault

instance (Traversable p, Traversable q, Foldable p, Foldable q) => Foldable (p :*: q) where
  foldMap = foldMapDefault

-- | tying the knot
newtype Fix f = InF (f (Fix f))

instance (Show (f (Fix f))) => Show (Fix f) where
  show (InF x) = "InF (" ++ show x ++ ")"

rec :: (Functor f) => (f v -> v) -> Fix f -> v
rec m (InF fd) =
  m
    ( fmap
        (rec m {- :: Fix f -> v -})
        (fd {- :: f (Fix f)-})
        {- :: f v -}
    )

instance Traversable I where
  traverse f (I x) = I <$> f x

instance Traversable (K a) where
  traverse _ (K c) = pure (K c)

instance (Traversable p, Traversable q) => Traversable (p :+: q) where
  traverse f (L px) = L <$> traverse f px
  traverse f (R qx) = R <$> traverse f qx

instance (Traversable p, Traversable q) => Traversable (p :*: q) where
  traverse f (px :*: qx) = (:*:) <$> traverse f px <*> traverse f qx

-- | makes fmap from traverse
instance Applicative I where
  pure = I
  I f <*> I s = I (f s)

-- | makes crush from traverse
instance (Monoid c) => Applicative (K c) where
  pure :: x -> K c x
  pure _ = K mempty
  (<*>) :: K c (s -> t) -> K c s -> K c t
  K f <*> K s = K (mappend f s)

crush :: (Traversable f, Monoid c) => (x -> c) -> f x -> c
crush m fx = unK $ traverse (K . m) fx

reduce :: (Traversable f, Monoid c) => f c -> c
reduce = crush id

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0

size :: (Functor f, Traversable f) => Fix f -> Int
size = rec ((1 +) . reduce)

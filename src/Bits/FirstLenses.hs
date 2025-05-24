{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Bits.FirstLenses
-- Description : Notes from "Lenses: Compositional Data Access and Manipulation"
--
-- Notes from Simon Peyton Jones's presentation,
--
-- /"Lenses: Compositional Data Access and Manipulation"/,
--
-- given at __Haskell Exchange 2013__, 2013.09.10
module Bits.FirstLenses where

-- * Motivation

data Person = P
  { _name :: String
  , _addr :: Address
  , _salary :: Int
  }
  deriving (Show)

data Address = A
  { _road :: String
  , _city :: String
  , _postcode :: String
  }
  deriving (Show)

-- $setup
-- >>> let addr = A { _road = "26 Bumblebee Ln", _city = "Manassis", _postcode = "02134"}
-- >>> let fred = P { _name = "Fred", _addr = addr, _salary = 100 }

-- $Why
-- This sort of code gets __tiresome__ really fast:
--
-- @
-- setName :: String -> Person -> Person
-- setName n p = p { _name = n }
--
-- setPostcode :: String -> Person -> Person
-- setPostcode pc p = p { _addr = (_addr p) { _postcode = pc } }
-- @

-- * Edward's Insight

-- ** One function to rule them all!

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> f t

-- |
-- Alternately,
--
-- @
-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
-- @
type Lens' s a = Lens s s a a

-- $Want
--
-- A lens for each field:
--
-- @
-- name     :: Lens' Person String
-- addr     :: Lens' Person Address
-- postcode :: Lens' Address String
-- @
--
-- A way to use the lens to get and update:
--
-- @
-- view :: Lens' s a -> s -> a
-- set  :: Lens' s a -> a -> s -> s
-- @
--
-- A way to compose lenses:
--
-- @
-- composeL :: Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a
-- @
--
-- If we had that...
--
-- @
-- setPostcode :: String -> Person -> Person
-- setPostcode pc p = set (addr `composeL` postcode) pc p
-- @

-- | A type which is isomorphic to `Lens'`
--
-- @
-- lensToLensR :: Lens' s a -> LensR s a
-- lensRToLens :: LensR s a -> Lens' s a
-- @
data LensR s a = L
  { viewR :: s -> a
  , setR :: a -> s -> s
  }

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- |
-- Point-free translation:
--
-- @
-- set :: Lens' s a -> a -> s -> s
-- set ln x = runIdentity . ln (Identity . const x)
-- @
set :: forall s a. Lens' s a -> a -> s -> s
set ln x s = runIdentity (ln setField s)
  where
    setField :: a -> Identity a
    setField _ = Identity x

over :: Lens' s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

-- | A functor that ignores its argument
newtype Const v a = Const {getConst :: v}

instance Functor (Const v) where
  fmap _ (Const x) = Const x

-- |
-- Point-free translation:
--
-- @
-- view :: Lens' s a -> s -> a
-- view ln = getConst . ln Const
-- @
view :: Lens' s a -> s -> a
view ln s = getConst (ln Const s)

lensToLensR :: Lens' s a -> LensR s a
lensToLensR ln = L {viewR = view ln, setR = set ln}

lensRToLens :: LensR s a -> Lens' s a
lensRToLens L {..} f s = (\a -> setR a s) <$> (f (viewR s))

-- * Making lenses

-- |
-- >>> view name fred
-- "Fred"
-- >>> set name "Bill" fred
-- P {_name = "Bill", _addr = A {_road = "26 Bumblebee Ln", _city = "Manassis", _postcode = "02134"}, _salary = 100}
name :: Lens' Person String
name eltFn (P n a s) = (\n' -> P n' a s) <$> (eltFn n)

-- * How on earth does this work?

-- $
--
-- > set name "Bill" fred
-- > runIdentity (name (\_ -> Identity "Bill") (P {_name = "Fred", _addr = ..., _salary = 100})) -- inline set
-- > runIdentity (fmap (\n' -> P n' A{...} 100) ((\_ -> Identity "Bill") "Fred"))                -- inline name
-- > P {_name = "Bill", _addr = A {_road = "26 Bumblebee Ln", _city = "Manassis", _postcode = "02134"}, _salary = 100}
--
-- > view name (P {_name = "Fred", _addr = ..., _salary = 100})
-- > getConst (name Const (P {_name = "Fred", _addr = ..., _salary = 100})) -- inline view
-- > getConst (fmap (\n' -> P n' A{...} 100) (Const "Fred"))                -- inline name
-- > getConst (Const "Fred")
-- > "Fred"
--
-- The __newtype__ has no runtime cost!
--
-- It just tells the @Functor f =>@ which functor dictionary to pass to `ln`.

-- * Composing and using lenses

address :: Lens' Person Address
address eltFn (P n a s) = (\a' -> P n a' s) <$> (eltFn a)

-- |
-- >>> view (address . postcode) fred
-- "02134"
postcode :: Lens' Address String
postcode eltFn (A r c p) = (\p' -> A r c p') <$> (eltFn p)

-- |
-- >>> setPostcode "55555" fred
-- P {_name = "Fred", _addr = A {_road = "26 Bumblebee Ln", _city = "Manassis", _postcode = "55555"}, _salary = 100}
setPostcode :: String -> Person -> Person
setPostcode = set (address . postcode)

-- * Edward's Second Insight

-- $
-- Change `Functor` to `Applicative` and get a multi-focus lens

type Traversal' s a = forall f. (Applicative f) => (a -> f a) -> s -> f s

roadAndCity :: Traversal' Address String
roadAndCity eltFn (A r c p) = (\r' c' -> A r' c' p) <$> eltFn r <*> eltFn c

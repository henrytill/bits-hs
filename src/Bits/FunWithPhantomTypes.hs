{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module : Bits.FunWithPhantomTypes
-- Description : Examples adapted from "Fun with phantom types"
--
-- Code adapted from R. Hinze's
-- /Fun with phantom types/
--
-- Here, Hinze introduces the concepts of GADTs, though by a different name, and
-- with a speculative syntax.
module Bits.FunWithPhantomTypes where

import Control.Monad (liftM, liftM2)
import Data.Char (ord)
import Data.List (unfoldr)
import Text.PrettyPrint.ANSI.Leijen hiding (list, pretty)

-- * 1. Introducing "phantom types"

-- | A simple expression language
data Term t where
  Zero :: Term Int
  Succ :: Term Int -> Term Int
  Pred :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a

-- | An intepreter for the expression language
--
-- >>> let one = Succ Zero
-- >>> :type one
-- one :: Term Int
-- >>> eval one
-- 1
-- >>> eval (IsZero one)
-- False
-- >>> eval (If (IsZero one) Zero one)
-- 1
-- >>> let true = IsZero Zero
-- >>> let false = IsZero one
-- >>> eval (If true true false)
-- True
eval :: forall t. Term t -> t
eval (Zero) = 0
eval (Succ e) = eval e + 1
eval (Pred e) = eval e - 1
eval (IsZero e) = eval e == 0
eval (If e1 e2 e3)
  | eval e1 = eval e2
  | otherwise = eval e3

-- * 2. Generic functions

-- $generic_functions
-- Suppose you are developing an application where the need arises to compress
-- data to strings of bits.  As it happens, you have data of many different
-- types and you want to program a compression function that works for all of
-- these types.  This sounds like a typical case for Hakell's type classes.
-- Alas, I promised to do without type classes.  Fortunately, phantom types
-- offer an intriguing alternative.
--
-- The basic idea is to define a type whose elements represent types.  For
-- concreteness, assume that we need compression functions for types built from
-- 'Int' and 'Char' using the list and the pair type constructor.

data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)
  RDyn :: Type Dynamic

deriving instance Show (Type t)

rString :: Type String
rString = RList RChar

type Bit = Int

-- toBinary :: Int -> [Bit]
-- toBinary 0 = []
-- toBinary x = (x `mod` 2) : (toBinary $ x `div` 2)

dec2bin :: Int -> [Bit]
dec2bin = unfoldr f
  where
    f 0 = Nothing
    f x = Just (x `mod` 2, x `div` 2)

padding :: Int -> Int -> [Bit]
padding p x
  | p <= len = b
  | otherwise = b ++ take (p - len) (repeat 0)
  where
    b = dec2bin x
    len = length b

compressInt :: Int -> [Bit]
compressInt = padding 32

compressChar :: Char -> [Bit]
compressChar = padding 7 . ord

data Rep = forall a. Rep (Type a)

compressRep :: Rep -> [Bit]
compressRep (Rep RInt) = [0, 0, 0]
compressRep (Rep RChar) = [0, 0, 1]
compressRep (Rep (RList ra)) = 0 : 1 : 0 : compressRep (Rep ra)
compressRep (Rep (RPair ra rb)) = 0 : 1 : 1 : compressRep (Rep ra) ++ compressRep (Rep rb)
compressRep (Rep RDyn) = [1, 0, 0]

compress :: forall t. Type t -> t -> [Bit]
compress RInt i = compressInt i
compress RChar c = compressChar c
compress (RList _) [] = 0 : []
compress (RList ra) (a : as) = 1 : compress ra a ++ compress (RList ra) as
compress (RPair ra rb) (a, b) = compress ra a ++ compress rb b
compress RDyn (Dyn ra x) = compressRep (Rep ra) ++ compress ra x

block :: Int -> Doc -> Doc
block i d = group (nest i d)

pretty :: forall t. Type t -> t -> Doc
pretty RInt i = int i
pretty RChar c = char c
pretty (RList RChar) s = text s
pretty (RList _) [] = text "[]"
pretty (RList ra) (a : as) = block 1 (text "[" <> pretty ra a <> prettyL as)
  where
    prettyL [] = text "]"
    prettyL (x : xs) = text "," <> line <> pretty ra x <> prettyL xs
pretty (RPair ra rb) (a, b) = text "(" <> pretty ra a <> text "," <> pretty rb b <> text ")"
pretty RDyn (Dyn ra x) = pretty ra x

-- * 3. Dynamic Values

-- |
-- >>> let ds = [Dyn RInt 60, Dyn rString "Bird"]
-- >>> :type ds
-- ds :: [Dynamic]
-- >>> Dyn (RList RDyn) ds
-- Dyn (RList RDyn) [Dyn RInt 60,Dyn (RList RChar) "Bird"]
-- >>> compress RDyn (Dyn (RList RDyn) ds)
-- [0,1,0,1,0,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,1,0,1,0,0,0,0,1,1,1,0,0,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,0,1,1,0,0]
data Dynamic = forall t. (Show t) => Dyn (Type t) t

deriving instance Show Dynamic

list :: (a -> b) -> [a] -> [b]
list = map

pair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
pair f g (a, b) = (f a, g b)

tequal :: forall t u. Type t -> Type u -> Maybe (t -> u)
tequal RInt RInt = return id
tequal RChar RChar = return id
tequal (RList ra1) (RList ra2) = liftM list (tequal ra1 ra2)
tequal (RPair ra1 rb1) (RPair ra2 rb2) = liftM2 pair (tequal ra1 ra2) (tequal rb1 rb2)
tequal _ _ = fail "cannot tequal"

-- |
-- >>> let d = Dyn RInt 60
-- >>> cast d RInt
-- Just 60
-- >>> cast d RChar
-- Nothing
cast :: forall t. Dynamic -> Type t -> Maybe t
cast (Dyn ra a) rt = fmap (\f -> f a) (tequal ra rt)

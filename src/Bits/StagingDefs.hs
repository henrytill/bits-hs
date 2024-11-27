{-# LANGUAGE TemplateHaskell #-}

module Bits.StagingDefs where

import Language.Haskell.TH.Lib (CodeQ)
import Language.Haskell.TH.Syntax (liftTyped)
import Prelude hiding (lookup)

power :: Int -> Int -> Int
power 0 _ = 1
power k n = n * power (k - 1) n

qpower :: Int -> CodeQ Int -> CodeQ Int
qpower 0 _ = [||1||]
qpower k n = [||$$(n) * $$(qpower (k - 1) n)||]

-- Example: staging lookup

data BST a
  = Node Int a (BST a) (BST a)
  | Leaf

lookup :: Int -> BST a -> Maybe a
lookup _ Leaf = Nothing
lookup i (Node j a l r) =
  case compare i j of
    LT -> lookup i l
    EQ -> Just a
    GT -> lookup i r

table :: BST String
table = Node 5 "b" (Node 3 "a" Leaf Leaf) (Node 7 "c" Leaf Leaf)

qtable :: BST (CodeQ String)
qtable = Node 5 [||"b"||] (Node 3 [||"a"||] Leaf Leaf) (Node 7 [||"c"||] Leaf Leaf)

qlookup :: CodeQ Int -> BST (CodeQ a) -> CodeQ (Maybe a)
qlookup _ Leaf = [||Nothing||]
qlookup i (Node j a l r) =
  [||
  case compare $$i $$(liftTyped j) of
    LT -> $$(qlookup i l)
    EQ -> Just $$a
    GT -> $$(qlookup i r)
  ||]

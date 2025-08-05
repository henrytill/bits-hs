{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Bits.TaggedAST where

import Control.Monad.State
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.DList as DList
import Data.Word (Word32)

type Ref = Word32

tagAndIndex :: Ref -> (# Word32, Word32 #)
tagAndIndex r = (# r .&. 7, r `shiftR` 3 #)

pattern LitRef :: Word32 -> Ref
pattern LitRef i <- (tagAndIndex -> (# 0, i #))
  where
    LitRef i = (i `shiftL` 3) .|. 0

pattern NegRef :: Word32 -> Ref
pattern NegRef i <- (tagAndIndex -> (# 1, i #))
  where
    NegRef i = (i `shiftL` 3) .|. 1

pattern NotRef :: Word32 -> Ref
pattern NotRef i <- (tagAndIndex -> (# 2, i #))
  where
    NotRef i = (i `shiftL` 3) .|. 2

pattern AddRef :: Word32 -> Ref
pattern AddRef i <- (tagAndIndex -> (# 3, i #))
  where
    AddRef i = (i `shiftL` 3) .|. 3

pattern XorRef :: Word32 -> Ref
pattern XorRef i <- (tagAndIndex -> (# 4, i #))
  where
    XorRef i = (i `shiftL` 3) .|. 4

{-# COMPLETE LitRef, NegRef, NotRef, AddRef, XorRef #-}

data AST = AST
  { literal :: !(UArray Word32 Int)
  , unary :: !(UArray Word32 Ref)
  , binary :: !(UArray Word32 Ref)
  }
  deriving (Show)

viewLit :: (# AST, Ref #) -> (# Int | () #)
viewLit (# ast, LitRef i #) = (# literal ast ! i | #)
viewLit _ = (# | () #)
{-# INLINE viewLit #-}

viewNeg :: (# AST, Ref #) -> (# Ref | () #)
viewNeg (# ast, NegRef i #) = (# unary ast ! i | #)
viewNeg _ = (# | () #)
{-# INLINE viewNeg #-}

viewNot :: (# AST, Ref #) -> (# Ref | () #)
viewNot (# ast, NotRef i #) = (# unary ast ! i | #)
viewNot _ = (# | () #)
{-# INLINE viewNot #-}

viewAdd :: (# AST, Ref #) -> (# (# Ref, Ref #) | () #)
viewAdd (# ast, AddRef i #) = (# (# binary ast ! i, binary ast ! (i + 1) #) | #)
viewAdd _ = (# | () #)
{-# INLINE viewAdd #-}

viewXor :: (# AST, Ref #) -> (# (# Ref, Ref #) | () #)
viewXor (# ast, XorRef i #) = (# (# binary ast ! i, binary ast ! (i + 1) #) | #)
viewXor _ = (# | () #)
{-# INLINE viewXor #-}

evaluate :: AST -> Ref -> Int
evaluate ast ref = case (# ast, ref #) of
  (viewLit -> (# lit | #)) -> lit
  (viewNeg -> (# operand | #)) -> negate $ evaluate ast operand
  (viewNot -> (# operand | #)) -> complement $ evaluate ast operand
  (viewAdd -> (# (# l, r #) | #)) -> evaluate ast l + evaluate ast r
  (viewXor -> (# (# l, r #) | #)) -> evaluate ast l `xor` evaluate ast r
  _ -> error "Invalid reference"

data BuilderState = BuilderState
  { literalCount :: !Word32
  , unaryCount :: !Word32
  , binaryCount :: !Word32
  , literalNodes :: !(DList Int)
  , unaryNodes :: !(DList Ref)
  , binaryNodes :: !(DList Ref)
  }
  deriving (Show)

initialState :: BuilderState
initialState = BuilderState 0 0 0 DList.empty DList.empty DList.empty

type Builder = State BuilderState

mkLiteral :: Int -> Builder Ref
mkLiteral val = do
  s <- get
  let idx = literalCount s
  put s {literalCount = idx + 1, literalNodes = literalNodes s `DList.snoc` val}
  return (LitRef idx)

mkNegate :: Ref -> Builder Ref
mkNegate val = do
  s <- get
  let idx = unaryCount s
  put s {unaryCount = idx + 1, unaryNodes = unaryNodes s `DList.snoc` val}
  return (NegRef idx)

mkNot :: Ref -> Builder Ref
mkNot val = do
  s <- get
  let idx = unaryCount s
  put s {unaryCount = idx + 1, unaryNodes = unaryNodes s `DList.snoc` val}
  return (NotRef idx)

mkAdd :: Ref -> Ref -> Builder Ref
mkAdd lhs rhs = do
  s <- get
  let idx = binaryCount s
  put s {binaryCount = idx + 2, binaryNodes = binaryNodes s `DList.snoc` lhs `DList.snoc` rhs}
  return (AddRef idx)

mkXor :: Ref -> Ref -> Builder Ref
mkXor lhs rhs = do
  s <- get
  let idx = binaryCount s
  put s {binaryCount = idx + 2, binaryNodes = binaryNodes s `DList.snoc` lhs `DList.snoc` rhs}
  return (XorRef idx)

freezeAST :: BuilderState -> AST
freezeAST s =
  AST
    { literal =
        if literalCount s > 0
          then listArray (0, literalCount s - 1) (DList.toList $ literalNodes s)
          else listArray (0, 0) [0]
    , unary =
        if unaryCount s > 0
          then listArray (0, unaryCount s - 1) (DList.toList $ unaryNodes s)
          else listArray (0, 0) [0]
    , binary =
        if binaryCount s > 0
          then listArray (0, binaryCount s - 1) (DList.toList $ binaryNodes s)
          else listArray (0, 0) [0]
    }

buildAST :: Builder Ref -> (# AST, Ref #)
buildAST builder =
  let (rootRef, finalState) = runState builder initialState
      ast = freezeAST finalState
   in (# ast, rootRef #)

prettyPrint :: AST -> Ref -> String
prettyPrint ast ref = case (# ast, ref #) of
  (viewLit -> (# lit | #)) -> show lit
  (viewNeg -> (# operand | #)) -> "-(" ++ prettyPrint ast operand ++ ")"
  (viewNot -> (# operand | #)) -> "~(" ++ prettyPrint ast operand ++ ")"
  (viewAdd -> (# (# l, r #) | #)) -> "(" ++ prettyPrint ast l ++ " + " ++ prettyPrint ast r ++ ")"
  (viewXor -> (# (# l, r #) | #)) -> "(" ++ prettyPrint ast l ++ " ^ " ++ prettyPrint ast r ++ ")"
  _ -> error "Invalid reference"

exampleExpression :: Builder Ref
exampleExpression = do
  five <- mkLiteral 5
  three <- mkLiteral 3
  sum1 <- mkAdd five three
  notSum <- mkNot sum1
  two <- mkLiteral 2
  one <- mkLiteral 1
  sum2 <- mkAdd two one
  negSum <- mkNegate sum2
  mkXor notSum negSum

test :: IO ()
test = do
  let (# ast, rootRef #) = buildAST exampleExpression
  putStrLn $ "AST: " ++ prettyPrint ast rootRef
  let result = evaluate ast rootRef
  putStrLn $ "Expression result: " ++ show result
  putStrLn $ "Root ref: " ++ show rootRef

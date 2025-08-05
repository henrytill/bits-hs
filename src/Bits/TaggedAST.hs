{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Bits.TaggedAST where

import Control.Monad.State
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.DList as DList
import Data.Word (Word32)

type Ref = Word32

tagAndIndex :: Ref -> (Word32, Word32)
tagAndIndex r = (r .&. 0b111, r `shiftR` 3)

makeRef :: Word32 -> Word32 -> Ref
makeRef tag i = (i `shiftL` 3) .|. tag

pattern LitRef :: Word32 -> Ref
pattern LitRef i <- (tagAndIndex -> (0, i))
  where
    LitRef i = makeRef 0 i

pattern NegRef :: Word32 -> Ref
pattern NegRef i <- (tagAndIndex -> (1, i))
  where
    NegRef i = makeRef 1 i

pattern NotRef :: Word32 -> Ref
pattern NotRef i <- (tagAndIndex -> (2, i))
  where
    NotRef i = makeRef 2 i

pattern AddRef :: Word32 -> Ref
pattern AddRef i <- (tagAndIndex -> (3, i))
  where
    AddRef i = makeRef 3 i

pattern XorRef :: Word32 -> Ref
pattern XorRef i <- (tagAndIndex -> (4, i))
  where
    XorRef i = makeRef 4 i

{-# COMPLETE LitRef, NegRef, NotRef, AddRef, XorRef #-}

data AST = AST
  { literal :: !(UArray Word32 Int)
  , unary :: !(UArray Word32 Ref)
  , binary :: !(UArray Word32 Ref)
  }
  deriving (Show)

viewLit :: (AST, Ref) -> Maybe Int
viewLit (ast, LitRef i) = Just (literal ast ! i)
viewLit _ = Nothing

viewNeg :: (AST, Ref) -> Maybe Ref
viewNeg (ast, NegRef i) = Just (unary ast ! i)
viewNeg _ = Nothing

viewNot :: (AST, Ref) -> Maybe Ref
viewNot (ast, NotRef i) = Just (unary ast ! i)
viewNot _ = Nothing

viewAdd :: (AST, Ref) -> Maybe (Ref, Ref)
viewAdd (ast, AddRef i) = Just (binary ast ! i, binary ast ! (i + 1))
viewAdd _ = Nothing

viewXor :: (AST, Ref) -> Maybe (Ref, Ref)
viewXor (ast, XorRef i) = Just (binary ast ! i, binary ast ! (i + 1))
viewXor _ = Nothing

evaluate :: AST -> Ref -> Int
evaluate ast = go
  where
    go ref = case (ast, ref) of
      (viewLit -> Just lit) -> lit
      (viewNeg -> Just operand) -> negate $ go operand
      (viewNot -> Just operand) -> complement $ go operand
      (viewAdd -> Just (l, r)) -> go l + go r
      (viewXor -> Just (l, r)) -> go l `xor` go r
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
mkLiteral i = do
  s <- get
  let idx = literalCount s
  put s {literalCount = idx + 1, literalNodes = literalNodes s `DList.snoc` i}
  return (LitRef idx)

mkNegate :: Ref -> Builder Ref
mkNegate r = do
  s <- get
  let idx = unaryCount s
  put s {unaryCount = idx + 1, unaryNodes = unaryNodes s `DList.snoc` r}
  return (NegRef idx)

mkNot :: Ref -> Builder Ref
mkNot r = do
  s <- get
  let idx = unaryCount s
  put s {unaryCount = idx + 1, unaryNodes = unaryNodes s `DList.snoc` r}
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
freezeAST s = AST {literal, unary, binary}
  where
    literal
      | literalCount s > 0 = listArray (0, literalCount s - 1) (DList.toList $ literalNodes s)
      | otherwise = listArray (0, 0) [0]
    unary
      | unaryCount s > 0 = listArray (0, unaryCount s - 1) (DList.toList $ unaryNodes s)
      | otherwise = listArray (0, 0) [0]
    binary
      | binaryCount s > 0 = listArray (0, binaryCount s - 1) (DList.toList $ binaryNodes s)
      | otherwise = listArray (0, 0) [0]

buildAST :: Builder Ref -> (AST, Ref)
buildAST builder =
  let (rootRef, finalState) = runState builder initialState
      ast = freezeAST finalState
   in (ast, rootRef)

prettyPrint :: AST -> Ref -> String
prettyPrint ast ref = case (ast, ref) of
  (viewLit -> Just lit) -> show lit
  (viewNeg -> Just operand) -> "-(" ++ prettyPrint ast operand ++ ")"
  (viewNot -> Just operand) -> "~(" ++ prettyPrint ast operand ++ ")"
  (viewAdd -> Just (l, r)) -> "(" ++ prettyPrint ast l ++ " + " ++ prettyPrint ast r ++ ")"
  (viewXor -> Just (l, r)) -> "(" ++ prettyPrint ast l ++ " ^ " ++ prettyPrint ast r ++ ")"
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
  let (ast, rootRef) = buildAST exampleExpression
  putStrLn $ "AST: " ++ prettyPrint ast rootRef
  let result = evaluate ast rootRef
  putStrLn $ "Expression result: " ++ show result
  putStrLn $ "Root ref: " ++ show rootRef

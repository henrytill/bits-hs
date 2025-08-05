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
tagAndIndex r = (r .&. 7, r `shiftR` 3)

pattern LitRef :: Word32 -> Ref
pattern LitRef i <- (tagAndIndex -> (0, i))
  where
    LitRef i = (i `shiftL` 3) .|. 0

pattern NegRef :: Word32 -> Ref
pattern NegRef i <- (tagAndIndex -> (1, i))
  where
    NegRef i = (i `shiftL` 3) .|. 1

pattern NotRef :: Word32 -> Ref
pattern NotRef i <- (tagAndIndex -> (2, i))
  where
    NotRef i = (i `shiftL` 3) .|. 2

pattern AddRef :: Word32 -> Ref
pattern AddRef i <- (tagAndIndex -> (3, i))
  where
    AddRef i = (i `shiftL` 3) .|. 3

pattern XorRef :: Word32 -> Ref
pattern XorRef i <- (tagAndIndex -> (4, i))
  where
    XorRef i = (i `shiftL` 3) .|. 4

{-# COMPLETE LitRef, NegRef, NotRef, AddRef, XorRef #-}

data AST = AST
  { literals :: !(UArray Word32 Int)
  , negates :: !(UArray Word32 Ref)
  , nots :: !(UArray Word32 Ref)
  , addLhs :: !(UArray Word32 Ref)
  , addRhs :: !(UArray Word32 Ref)
  , xorLhs :: !(UArray Word32 Ref)
  , xorRhs :: !(UArray Word32 Ref)
  }
  deriving (Show)

viewLit :: (AST, Ref) -> Maybe Int
viewLit (ast, LitRef i) = Just (literals ast ! i)
viewLit _ = Nothing
{-# INLINE viewLit #-}

viewNeg :: (AST, Ref) -> Maybe Ref
viewNeg (ast, NegRef i) = Just (negates ast ! i)
viewNeg _ = Nothing
{-# INLINE viewNeg #-}

viewNot :: (AST, Ref) -> Maybe Ref
viewNot (ast, NotRef i) = Just (nots ast ! i)
viewNot _ = Nothing
{-# INLINE viewNot #-}

viewAdd :: (AST, Ref) -> Maybe (Ref, Ref)
viewAdd (ast, AddRef i) = Just (addLhs ast ! i, addRhs ast ! i)
viewAdd _ = Nothing
{-# INLINE viewAdd #-}

viewXor :: (AST, Ref) -> Maybe (Ref, Ref)
viewXor (ast, XorRef i) = Just (xorLhs ast ! i, xorRhs ast ! i)
viewXor _ = Nothing
{-# INLINE viewXor #-}

evaluate :: AST -> Ref -> Int
evaluate ast ref = case (ast, ref) of
  (viewLit -> Just literal) -> literal
  (viewNeg -> Just operand) -> negate $ evaluate ast operand
  (viewNot -> Just operand) -> complement $ evaluate ast operand
  (viewAdd -> Just (l, r)) -> evaluate ast l + evaluate ast r
  (viewXor -> Just (l, r)) -> evaluate ast l `xor` evaluate ast r
  _ -> error "Invalid reference"

data BuilderState = BuilderState
  { litCount :: !Word32
  , negCount :: !Word32
  , notCount :: !Word32
  , addCount :: !Word32
  , xorCount :: !Word32
  , litNodes :: !(DList Int)
  , negNodes :: !(DList Ref)
  , notNodes :: !(DList Ref)
  , addLhsNodes :: !(DList Ref)
  , addRhsNodes :: !(DList Ref)
  , xorLhsNodes :: !(DList Ref)
  , xorRhsNodes :: !(DList Ref)
  }
  deriving (Show)

initialState :: BuilderState
initialState = BuilderState 0 0 0 0 0 DList.empty DList.empty DList.empty DList.empty DList.empty DList.empty DList.empty

type Builder = State BuilderState

mkLiteral :: Int -> Builder Ref
mkLiteral val = do
  s <- get
  let idx = litCount s
  put s {litCount = idx + 1, litNodes = litNodes s `DList.snoc` val}
  return (LitRef idx)

mkNegate :: Ref -> Builder Ref
mkNegate rhs = do
  s <- get
  let idx = negCount s
  put s {negCount = idx + 1, negNodes = negNodes s `DList.snoc` rhs}
  return (NegRef idx)

mkNot :: Ref -> Builder Ref
mkNot rhs = do
  s <- get
  let idx = notCount s
  put s {notCount = idx + 1, notNodes = notNodes s `DList.snoc` rhs}
  return (NotRef idx)

mkAdd :: Ref -> Ref -> Builder Ref
mkAdd lhs rhs = do
  s <- get
  let idx = addCount s
  put s {addCount = idx + 1, addLhsNodes = addLhsNodes s `DList.snoc` lhs, addRhsNodes = addRhsNodes s `DList.snoc` rhs}
  return (AddRef idx)

mkXor :: Ref -> Ref -> Builder Ref
mkXor lhs rhs = do
  s <- get
  let idx = xorCount s
  put s {xorCount = idx + 1, xorLhsNodes = xorLhsNodes s `DList.snoc` lhs, xorRhsNodes = xorRhsNodes s `DList.snoc` rhs}
  return (XorRef idx)

freezeAST :: BuilderState -> AST
freezeAST s =
  AST
    { literals =
        if litCount s > 0
          then listArray (0, litCount s - 1) (DList.toList $ litNodes s)
          else listArray (0, 0) [0]
    , negates =
        if negCount s > 0
          then listArray (0, negCount s - 1) (DList.toList $ negNodes s)
          else listArray (0, 0) [LitRef 0]
    , nots =
        if notCount s > 0
          then listArray (0, notCount s - 1) (DList.toList $ notNodes s)
          else listArray (0, 0) [LitRef 0]
    , addLhs =
        if addCount s > 0
          then listArray (0, addCount s - 1) (DList.toList $ addLhsNodes s)
          else listArray (0, 0) [LitRef 0]
    , addRhs =
        if addCount s > 0
          then listArray (0, addCount s - 1) (DList.toList $ addRhsNodes s)
          else listArray (0, 0) [LitRef 0]
    , xorLhs =
        if xorCount s > 0
          then listArray (0, xorCount s - 1) (DList.toList $ xorLhsNodes s)
          else listArray (0, 0) [LitRef 0]
    , xorRhs =
        if xorCount s > 0
          then listArray (0, xorCount s - 1) (DList.toList $ xorRhsNodes s)
          else listArray (0, 0) [LitRef 0]
    }

buildAST :: Builder Ref -> (AST, Ref)
buildAST builder =
  let (rootRef, finalState) = runState builder initialState
      ast = freezeAST finalState
   in (ast, rootRef)

prettyPrint :: AST -> Ref -> String
prettyPrint ast ref = case (ast, ref) of
  (viewLit -> Just literal) -> show literal
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

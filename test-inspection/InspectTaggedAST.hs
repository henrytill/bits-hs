{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module InspectTaggedAST (main) where

import Bits.TaggedAST (AST (..), Ref, buildAST, exampleExpression, prettyPrint)
import Data.Array.Unboxed ((!))
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Word (Word32)
import Test.Inspection

invalidReference :: Int
invalidReference = error "Invalid reference"

-- Boxed

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
evaluate ast ref = case (ast, ref) of
  (viewLit -> Just lit) -> lit
  (viewNeg -> Just operand) -> negate $ evaluate ast operand
  (viewNot -> Just operand) -> complement $ evaluate ast operand
  (viewAdd -> Just (l, r)) -> evaluate ast l + evaluate ast r
  (viewXor -> Just (l, r)) -> evaluate ast l `xor` evaluate ast r
  _ -> invalidReference

-- Unboxed

tagAndIndexU :: Ref -> (# Word32, Word32 #)
tagAndIndexU r = (# r .&. 7, r `shiftR` 3 #)
{-# INLINE tagAndIndexU #-}

pattern LitRefU :: Word32 -> Ref
pattern LitRefU i <- (tagAndIndexU -> (# 0, i #))
  where
    LitRefU i = (i `shiftL` 3) .|. 0
{-# INLINE LitRefU #-}

pattern NegRefU :: Word32 -> Ref
pattern NegRefU i <- (tagAndIndexU -> (# 1, i #))
  where
    NegRefU i = (i `shiftL` 3) .|. 1
{-# INLINE NegRefU #-}

pattern NotRefU :: Word32 -> Ref
pattern NotRefU i <- (tagAndIndexU -> (# 2, i #))
  where
    NotRefU i = (i `shiftL` 3) .|. 2
{-# INLINE NotRefU #-}

pattern AddRefU :: Word32 -> Ref
pattern AddRefU i <- (tagAndIndexU -> (# 3, i #))
  where
    AddRefU i = (i `shiftL` 3) .|. 3
{-# INLINE AddRefU #-}

pattern XorRefU :: Word32 -> Ref
pattern XorRefU i <- (tagAndIndexU -> (# 4, i #))
  where
    XorRefU i = (i `shiftL` 3) .|. 4
{-# INLINE XorRefU #-}

{-# COMPLETE LitRefU, NegRefU, NotRefU, AddRefU, XorRefU #-}

viewLitU :: (# AST, Ref #) -> (# Int | () #)
viewLitU (# ast, LitRefU i #) = (# literal ast ! i | #)
viewLitU _ = (# | () #)
{-# INLINE viewLitU #-}

viewNegU :: (# AST, Ref #) -> (# Ref | () #)
viewNegU (# ast, NegRefU i #) = (# unary ast ! i | #)
viewNegU _ = (# | () #)
{-# INLINE viewNegU #-}

viewNotU :: (# AST, Ref #) -> (# Ref | () #)
viewNotU (# ast, NotRefU i #) = (# unary ast ! i | #)
viewNotU _ = (# | () #)
{-# INLINE viewNotU #-}

viewAddU :: (# AST, Ref #) -> (# (# Ref, Ref #) | () #)
viewAddU (# ast, AddRefU i #) = (# (# binary ast ! i, binary ast ! (i + 1) #) | #)
viewAddU _ = (# | () #)
{-# INLINE viewAddU #-}

viewXorU :: (# AST, Ref #) -> (# (# Ref, Ref #) | () #)
viewXorU (# ast, XorRefU i #) = (# (# binary ast ! i, binary ast ! (i + 1) #) | #)
viewXorU _ = (# | () #)
{-# INLINE viewXorU #-}

evaluateU :: AST -> Ref -> Int
evaluateU ast ref = case (# ast, ref #) of
  (viewLitU -> (# lit | #)) -> lit
  (viewNegU -> (# operand | #)) -> negate $ evaluateU ast operand
  (viewNotU -> (# operand | #)) -> complement $ evaluateU ast operand
  (viewAddU -> (# (# l, r #) | #)) -> evaluateU ast l + evaluateU ast r
  (viewXorU -> (# (# l, r #) | #)) -> evaluateU ast l `xor` evaluateU ast r
  _ -> invalidReference

inspect $ 'evaluate === 'evaluateU

main :: IO ()
main = do
  let (ast, rootRef) = buildAST exampleExpression
  putStrLn $ "AST: " ++ prettyPrint ast rootRef
  putStrLn $ "Root ref: " ++ show rootRef
  let result = evaluate ast rootRef
  putStrLn $ "Expression result: " ++ show result
  let resultU = evaluateU ast rootRef
  putStrLn $ "Expression result (unboxed): " ++ show resultU

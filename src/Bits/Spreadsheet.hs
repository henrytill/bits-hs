{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Bits.Spreadsheet
  ( Cell
  , Exp
  , runExp
  , IdSupply
  , mkIdSupply
  , mkCell
  , get
  , set
  , liftIO
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.List as List

-- | The type of cells containing a value of type 'a'
data Cell a = Cell
  { code :: IORef (Exp a)
  , value :: IORef (Maybe a)
  , reads :: IORef [ECell]
  , observers :: IORef [ECell]
  , id :: Int
  }

data ECell where
  Pack :: Cell a -> ECell

instance Eq ECell where
  Pack a == Pack b = a.id == b.id

-- | The ID supply
newtype IdSupply = IdSupply (IORef Int)

-- | Creates a new ID supply
mkIdSupply :: IO IdSupply
mkIdSupply = IdSupply <$> newIORef 0

idFromSupply :: IdSupply -> IO Int
idFromSupply (IdSupply ref) = atomicModifyIORef ref $ \n -> let n' = succ n in (n', n')

-- | The type of expressions that return a value of type 'a'
newtype Exp a = Exp ((?idSupply :: IdSupply) => IO (a, [ECell]))

-- | Runs an expression
runExp :: Exp a -> IdSupply -> IO a
runExp (Exp cmd) idSupply =
  let ?idSupply = idSupply
   in fst <$> cmd

instance MonadIO Exp where
  liftIO action = Exp $ (,[]) <$> action

instance Functor Exp where
  fmap f (Exp m) = Exp $ do
    (a, cs) <- m
    return (f a, cs)

instance Applicative Exp where
  pure v = Exp $ return (v, [])
  (<*>) = ap

instance Monad Exp where
  (Exp cmd) >>= f = Exp $ do
    (a, cs) <- cmd
    let Exp cmd' = f a
    (b, ds) <- cmd'
    return (b, List.union cs ds)

-- | Creates a new cell (uses implicit environment)
mkCell :: Exp a -> Exp (Cell a)
mkCell e = Exp $ do
  n <- idFromSupply ?idSupply
  codeRef <- newIORef e
  valueRef <- newIORef Nothing
  readsRef <- newIORef []
  observersRef <- newIORef []
  let c = Cell codeRef valueRef readsRef observersRef n
  return (c, [])

-- | Reads a cell
get :: Cell a -> Exp a
get c = Exp $ do
  maybeValue <- readIORef c.value
  case maybeValue of
    -- value is memoized
    Just v -> return (v, [Pack c])
    -- value must be recomputed
    Nothing -> do
      Exp code <- readIORef c.code
      (v, ds) <- code
      writeIORef c.value (Just v)
      writeIORef c.reads ds
      forM_ ds $ \(Pack d) ->
        modifyIORef d.observers (Pack c :)
      return (v, [Pack c])

removeObserver :: ECell -> ECell -> IO ()
removeObserver o (Pack c) = modifyIORef c.observers $ filter (/= o)

invalidate :: ECell -> IO ()
invalidate ec@(Pack c) = do
  os <- readIORef c.observers
  rs <- readIORef c.reads
  writeIORef c.observers []
  writeIORef c.value Nothing
  writeIORef c.reads []
  mapM_ (removeObserver ec) rs
  mapM_ invalidate os

-- | Modifies the contents of a cell
set :: Cell a -> Exp a -> Exp ()
set c e = liftIO $ do
  writeIORef c.code e
  invalidate (Pack c)

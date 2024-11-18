{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bits.AwkwardSquad where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar hiding (readMVar)
import Control.Monad (forever)
import Prelude hiding (read)

type Channel a =
  ( MVar (Stream a), -- Read end
    MVar (Stream a) -- Write end (the hole)
  )

type Stream a = MVar (Item a)

data Item a = MkItem a (Stream a)

newChan :: IO (Channel a)
newChan = do
  read <- newEmptyMVar
  write <- newEmptyMVar
  hole <- newEmptyMVar
  putMVar read hole
  putMVar write hole
  return (read, write)

putChan :: Channel a -> a -> IO ()
putChan (_, write) val = do
  new_hole <- newEmptyMVar
  old_hole <- takeMVar write
  putMVar write new_hole
  putMVar old_hole (MkItem val new_hole)

getChan :: Channel a -> IO a
getChan (read, _) = do
  head_var <- takeMVar read
  MkItem val new_head <- readMVar head_var
  putMVar read new_head
  return val

dupChan :: Channel a -> IO (Channel a)
dupChan (_, write) = do
  new_read <- newEmptyMVar
  hole <- readMVar write
  putMVar new_read hole
  return (new_read, write)

readMVar :: MVar a -> IO a
readMVar var = do
  val <- takeMVar var
  putMVar var val
  return val

main :: IO ()
main = do
  c <- newChan
  _ <- forkIO (forever (getChan c >>= print))
  mapM_ (putChan c) [9, 8 .. 0]

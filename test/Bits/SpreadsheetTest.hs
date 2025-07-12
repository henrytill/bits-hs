{-# LANGUAGE ImplicitParams #-}

module Bits.SpreadsheetTest where

import Bits.Spreadsheet
import Text.Printf

example :: Exp ()
example = do
  x <- mkCell (return (10 :: Int))
  y <- mkCell (return (20 :: Int))
  result <- mkCell (return (0 :: Int))

  set result ((+) <$> get x <*> get y)

  a1 <- get result
  liftIO $ printf "%d\n" a1

  set x (return 20)
  a2 <- get result
  liftIO $ printf "%d\n" a2

  set y (return 30)
  a3 <- get result
  liftIO $ printf "%d\n" a3

  return ()

main :: IO ()
main = mkIdSupply >>= runExp example

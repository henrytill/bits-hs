{-# LANGUAGE TemplateHaskell #-}

module Bits.Staging where

import Bits.StagingDefs

power5 :: Int -> Int
power5 n = $$(qpower 5 [||n||])

lookupTable :: Int -> Maybe String
lookupTable i = $$(qlookup [||i||] qtable)

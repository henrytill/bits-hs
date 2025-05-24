{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Bits.StagedRouterExample where

import Bits.StagedRouter
import Data.Text (Text)
import qualified Data.Text as T

routeRequest :: Request -> Response
routeRequest req = $$(routeViaTree exampleTree [||req||] [||[]||])

main :: IO ()
main = do
  putStrLn "\nTesting routes:"
  mapM_
    testRoute
    [ ["login"]
    , ["login", "bob"]
    , ["language"]
    , ["language", "haskell"]
    , ["language", "haskell", "new"]
    , ["language", "haskell", "feature"]
    , ["language", "haskell", "feature", "typed-th"]
    , ["language", "haskell", "feature", "typed-th", "since"]
    , ["invalid", "path"]
    ]
  where
    testRoute :: [Text] -> IO ()
    testRoute path = do
      putStrLn $ "Path: /" <> T.unpack (T.intercalate "/" path)
      putStrLn $ "Response: " <> T.unpack (routeRequest path)
      putStrLn ""

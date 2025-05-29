{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bits.StagedRouter where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Language.Haskell.TH.Lib (CodeQ)
import Language.Haskell.TH.Syntax (liftTyped)

-- | Route definition structure
data Route
  = -- | Static path component
    Static Text Route
  | -- | Variable capture
    Capture Route
  | -- | End of route
    End
  deriving (Show)

-- | Basic types for request handling
type Request = [Text]

type Response = Text

type Handler = Request -> Response

-- | Router containing route/handler pairs
newtype Router = MkRouter [(Route, CodeQ Handler)]

instance Semigroup Router where
  MkRouter a <> MkRouter b = MkRouter (a <> b)

instance Monoid Router where
  mempty = MkRouter []

-- | Helper to add a route
at :: Route -> CodeQ Handler -> Router
at route handler = MkRouter [(route, handler)]

-- | Optimized routing tree
data RouteTree
  = RouteTreeNode
      (Map Text RouteTree) -- dispatch on topmost path component
      (Maybe RouteTree) -- routes that capture this component
      (CodeQ Handler) -- possibly failing handler for current path

defaultTree :: RouteTree
defaultTree = RouteTreeNode Map.empty Nothing [||const "404"||]

insertRoute :: (Route, CodeQ Handler) -> RouteTree -> RouteTree
insertRoute (r, h) = go r h []
  where
    go :: Route -> CodeQ Handler -> [Text] -> RouteTree -> RouteTree
    -- For a static component, recurse into the static map
    go (Static text rest) handler captures (RouteTreeNode statics wildcards defaultHandler) =
      RouteTreeNode
        (Map.alter (Just . insertIntoStatic) text statics)
        wildcards
        defaultHandler
      where
        insertIntoStatic Nothing = go rest handler captures defaultTree
        insertIntoStatic (Just tree) = go rest handler captures tree
    -- For a capture, recurse into the wildcard branch
    go (Capture rest) handler captures (RouteTreeNode statics wildcards defaultHandler) =
      RouteTreeNode
        statics
        (Just $ maybe (go rest handler captures defaultTree) (go rest handler captures) wildcards)
        defaultHandler
    -- At the end of a route, install the handler
    go End handler _ (RouteTreeNode statics wildcards _) =
      RouteTreeNode
        statics
        wildcards
        handler

-- | Convert Router to RouteTree for efficient dispatch
buildRouteTree :: Router -> RouteTree
buildRouteTree (MkRouter routes) = foldr insertRoute defaultTree routes

routeViaTree :: RouteTree -> CodeQ Request -> CodeQ [Text] -> CodeQ Response
routeViaTree (RouteTreeNode statics captures handler) req args =
  [||
  case $$req of
    [] -> $$handler $$args
    x : xs -> $$(go (Map.toList statics) captures [||x||] [||xs||])
  ||]
  where
    go :: [(Text, RouteTree)] -> Maybe RouteTree -> CodeQ Text -> CodeQ Request -> CodeQ Response
    go [] Nothing _ _ = [||"404"||]
    go [] (Just cs) x xs = routeViaTree cs xs [||$$x : $$args||]
    go ((y, tree) : st) _ x xs =
      [||
      if $$x == $$(liftTyped y)
        then $$(routeViaTree tree xs args)
        else $$(go st captures x xs)
      ||]

exampleRouter :: Router
exampleRouter =
  mconcat
    [ at
        (Static "login" End)
        [||const "Login page"||]
    , at
        (Static "language" End)
        [||const "List of all languages"||]
    , at
        (Static "language" (Capture End))
        [||\(lid : _) -> "Details for language: " <> lid||]
    , at
        (Static "language" (Capture (Static "new" End)))
        [||\(lid : _) -> "New feature form for language: " <> lid||]
    , at
        (Static "language" (Capture (Static "feature" End)))
        [||\(lid : _) -> "Features for language: " <> lid||]
    , at
        (Static "language" (Capture (Static "feature" (Capture End))))
        [||\(lid : fid : _) -> "Feature " <> fid <> " for language: " <> lid||]
    , at
        (Static "language" (Capture (Static "feature" (Capture (Static "since" End)))))
        [||\(lid : fid : _) -> "Version history for feature " <> fid <> " in language: " <> lid||]
    , at
        (Static "login" (Capture End))
        [||\(name : _) -> "Login for: " <> name||]
    ]

exampleTree :: RouteTree
exampleTree = buildRouteTree exampleRouter

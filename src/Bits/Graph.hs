-- |
-- Module      : Bits.Graph
-- Description : Structuring Depth-First Search Algorithms in Haskell
--
-- Code from David J. King and John Launchbury's
-- /Structuring Depth-First Search Algorithms in Haskell/.
module Bits.Graph where

import Data.Array

type Vertex = Char

type Table a = Array Vertex a

type Graph = Table [Vertex]

vertices :: Graph -> [Vertex]
vertices = indices

type Edge = (Vertex, Vertex)

edges :: Graph -> [Edge]
edges g = [(v, w) | v <- vertices g, w <- g ! v]

mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t ! v)) | v <- indices t]

type Bounds = (Vertex, Vertex)

outdegree :: Graph -> Table Int
outdegree g = mapT numEdges g
  where
    numEdges _ ws = length ws

buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

graph :: Graph
graph =
  buildG
    ('a', 'j')
    [ ('a', 'j')
    , ('a', 'g')
    , ('b', 'i')
    , ('b', 'a')
    , ('c', 'h')
    , ('c', 'e')
    , ('e', 'j')
    , ('e', 'h')
    , ('e', 'd')
    , ('f', 'i')
    , ('g', 'f')
    , ('g', 'b')
    ]

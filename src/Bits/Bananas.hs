{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- Module      : Bits.Bananas
-- Description : Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire
--
-- <https://www.researchgate.net/publication/2592417_Functional_Programming_with_Bananas_Lenses_Envelopes_and_Barbed_Wire>
--
-- References:
--
--     * <https://en.wikipedia.org/wiki/Bird%E2%80%93Meertens_formalism>
--
--     * <https://en.wikipedia.org/wiki/Catamorphism>
--
--     * <https://en.wikipedia.org/wiki/Anamorphism>
--
--     * <https://en.wikipedia.org/wiki/Paramorphism>
--
--     * <https://en.wikipedia.org/wiki/Hylomorphism_(computer_science)>
--
--     * <http://blog.ezyang.com/2010/05/bananas-lenses-envelopes-and-barbed-wire-a-translation-guide/>
module Bits.Bananas where

import Prelude hiding (filter, foldr, iterate, length, map, zip, (||))

-- * 2. The data type of lists

data List a
  = Nil
  | Cons (a, (List a))
  deriving (Eq, Show)

xs, ys :: List Int

-- |
-- >>> xs
-- Cons (1,Cons (2,Cons (3,Nil)))
xs = Cons (1, Cons (2, Cons (3, Nil)))

-- |
-- >>> ys
-- Cons (3,Cons (2,Cons (1,Nil)))
ys = Cons (3, Cons (2, Cons (1, Nil)))

-- ** Catamorphisms

type Cata a b = (b, a -> b -> b)

-- |
-- >>> foldr (0, (+)) xs
-- 6
foldr :: Cata a b -> List a -> b
foldr (b, _) Nil = b
foldr h@(_, op) (Cons (a, as)) = a `op` (foldr h as)

-- |
-- >>> length xs
-- 3
length :: List a -> Integer
length = foldr (0, op)
  where
    _ `op` n = 1 + n

-- |
-- >>> filter odd xs
-- Cons (1,Cons (3,Nil))
filter :: (a -> Bool) -> List a -> List a
filter p = foldr (Nil, op)
  where
    a `op` as
      | p a = Cons (a, as)
      | otherwise = as

-- ** Anamorphisms

-- $anamorphisms
--
-- @
-- Anamorphisms are not well-known in the functional programming folklore.  They
-- are called `unfold` by Bird & Wadler, who spend only a few words on them.
-- @

type Ana a b = (b -> (a, b), b -> Bool)

-- | Given a predicate `p` and a function `g`, `unfold` represents a list-anamorphism.
unfold :: Ana a b -> b -> List a
unfold h@(g, p) b
  | p b = Nil
  | otherwise = Cons (a, unfold h b')
  where
    (a, b') = g b

-- | Zips a pair of `List`s into a `List` of pairs
--
-- >>> zip (xs, ys)
-- Cons ((1,3),Cons ((2,2),Cons ((3,1),Nil)))
zip :: (List x, List y) -> List (x, y)
zip = unfold (g, p)
  where
    p (Nil, _) = True
    p (_, Nil) = True
    p _ = False

    g (Cons (a, as), Cons (b, bs)) = ((a, b), (as, bs))

-- | Constructs the infinite list of iterated applications of `f` to `a`
iterate :: (a -> a) -> a -> List a
iterate f = unfold (g, const False)
  where
    g a = (a, f a)

-- | Applies `f` to every element in a given `List`.
--
-- >>> map (+1) xs
-- Cons (2,Cons (3,Cons (4,Nil)))
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons (a, as)) = Cons (f a, map f as)

-- | Applies `f` to every element in a given `List` (Catamorphic version)
--
-- >>> cataMap (+1) xs
-- Cons (2,Cons (3,Cons (4,Nil)))
cataMap :: (a -> b) -> List a -> List b
cataMap f = foldr (Nil, op)
  where
    a `op` bs = Cons (f a, bs)

-- | Applies `f` to every element in a given `List` (Anamorphic version)
--
-- >>> anaMap (+1) xs
-- Cons (2,Cons (3,Cons (4,Nil)))
anaMap :: (a -> b) -> List a -> List b
anaMap f = unfold (g, p)
  where
    p Nil = True
    p _ = False

    g (Cons (a, as)) = (f a, as)

-- ** Hylomorphisms

-- $hylomorphisms
--
-- @
-- A recursive function whose call-tree is isomorphic to a cons-list, i.e. a
-- linear recursive function, is called a /hylomorphism/.
-- @
--
-- @
-- A hylomorphism corresponds to the composition of an anamorphism that builds
-- the call-tree as an explicit data structure and a catamorphism that reduces
-- this data object to the required value.
-- @

-- type Hylo a b c = ((c, b -> c -> c), (a -> (b, a), a -> Bool))
type Hylo a b c = ((Cata b c), (Ana b a))

unfoldAndFold :: Hylo a b c -> a -> c
unfoldAndFold h@((c, op), (g, p)) a
  | p a = c
  | otherwise = b `op` unfoldAndFold h a'
  where
    (b, a') = g a

-- | An archetypal hylomorphism is the `factorial` function:
--
-- >>> fac 5
-- 120
fac :: Integer -> Integer
fac = unfoldAndFold ((1, (*)), (g, p))
  where
    p 0 = True
    p _ = False

    g n = (n, n - 1)

-- ** Paramorphisms

-- $paramorphism
--
-- @...cover (the) pattern of primitive recursion.@
--
-- From <https://en.wikipedia.org/wiki/Paramorphism Wikipedia>:
--
-- @
-- A paramorphism is an extension of the concept of catamorphism first
-- introduced by Lambert Meertens to deal with a form which "eats its argument
-- and keeps it too", as exemplified by the factorial function.
-- @

type Para a b c = (b, a -> c -> b)

numPara :: (Eq a, Num a) => Para a b b -> a -> b
numPara (b, _) 0 = b
numPara h@(_, op) n = (n - 1) `op` (numPara h (n - 1))

-- |
-- >>> fac' 5
-- 120
fac' :: Integer -> Integer
fac' = numPara (1, op)
  where
    n `op` m = (1 + n) * m

listPara :: Para a b (List a, b) -> List a -> b
listPara (b, _) Nil = b
listPara h@(_, op) (Cons (a, as)) = a `op` (as, listPara h as)

-- |
-- >>> tails xs
-- Cons (Cons (1,Cons (2,Cons (3,Nil))),Cons (Cons (2,Cons (3,Nil)),Cons (Cons (3,Nil),Cons (Nil,Nil))))
tails :: List a -> List (List a)
tails = listPara (Cons (Nil, Nil), op)
  where
    a `op` (as, tls) = Cons (Cons (a, as), tls)

-- * 3. Algebraic data types

-- $algebraic
--
-- @
-- In order to define the notions of cata-, ana-, hylo-, and paramorphism for
-- arbitrary datatypes, we now present a generic theory of data types and
-- functions on them.  For this we consider a recurive data type (also called
-- 'algebraic' data types in Miranda) to be defined as the least fixed point of
-- a functor.
-- @

-- ** Functors

-- *** Product

(***) :: (a -> b) -> (a' -> b') -> (a, a') -> (b, b')
f *** g = \(x, x') -> (f x, g x')

(&&&) :: (a -> b) -> (a -> b') -> a -> (b, b')
f &&& g = \x -> (f x, g x)

-- *** Sum

-- *** Arrow

-- *** Identity, Constants

-- *** Lifting

-- *** Sectioning

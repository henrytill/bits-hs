{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Bits.Origami
-- Description : Origami programming
--
-- <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/origami.pdf>
module Bits.Origami where

import Data.Maybe (isNothing)
import Prelude hiding (repeat, zip)

-- * Origami with lists: sorting

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- $setup
-- >>> let x = Cons 1 (Cons 2 (Cons 3 Nil))
-- >>> let y = Cons 4 (Cons 5 (Cons 6 Nil))

-- | A constructor for singleton `List`s
--
-- >>> wrap 7
-- Cons 7 Nil
wrap :: a -> List a
wrap x = Cons x Nil

-- | Detects empty lists
--
-- >>> nil Nil
-- True
-- >>> nil (Cons 1 Nil)
-- False
nil :: List a -> Bool
nil Nil = True
nil (Cons _ _) = False

-- $
-- =Note:
-- Unfolds /generate/ data structures and folds /consume/ them.

-- ** Folds for lists

-- | A natural fold for lists.  This is equivalent to `foldr`
--
-- >>> foldL (+) 0 x
-- 6
foldL :: (a -> b -> b) -> b -> List a -> b
foldL _ e Nil = e
foldL f e (Cons x xs) = f x (foldL f e xs)

-- *** Exercise 3.2

-- |
-- >>> mapL (+1) x
-- Cons 2 (Cons 3 (Cons 4 Nil))
mapL :: (a -> b) -> List a -> List b
mapL f = foldL (\a acc -> Cons (f a) acc) Nil

-- |
-- >>> appendL x y
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
appendL :: List a -> List a -> List a
appendL xs ys = foldL Cons ys xs

-- |
-- >>> let xy = Cons x (Cons y Nil)
-- >>> concatL xy
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
concatL :: List (List a) -> List a
concatL = foldL appendL Nil

-- *** Exercise 3.3

-- $
--
-- >>> foldL (+) 0 . mapL (*5) $ x
-- 30
-- >>> foldL ((+) . (*5)) 0 $ x
-- 30

-- | A classic application of of `foldL` - the insertion sort algorithm.
--
-- IPFH defines `insert` using `takeWhile` and `dropWhile`, but we make the
-- recursion pattern explicit so that we cant study it.
--
-- >>> isort (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
isort :: (Ord a) => List a -> List a
isort = foldL insert Nil
  where
    insert :: (Ord a) => a -> List a -> List a
    insert y Nil = wrap y
    insert y (Cons x xs)
      | y < x = Cons y (Cons x xs)
      | otherwise = Cons x (insert y xs)

-- *** Exercise 3.4

-- |
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insert1 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
insert1 :: (Ord a) => a -> List a -> List a
insert1 y = snd . foldL inserter (Nil, wrap y)
  where
    inserter x (xs, acc)
      | y < x = (Cons x xs, Cons y (Cons x xs))
      | otherwise = (Cons x xs, Cons x acc)

-- |
-- >>> isort1 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
isort1 :: (Ord a) => List a -> List a
isort1 = foldL insert1 Nil

-- *** Exercise 3.5

-- $
-- A paramorphism captures a recursion pattern where the result depends not only
-- on a recursive call on a substructure, but also on the substructure itself.

-- | The paramorphism operator for a list.  The argument `f` takes a copy of the
-- tail `xs` along with the result `paraL f e xs` of the recursive call on that
-- tail.
paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL _ e Nil = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)

-- | The paramorphism operator for numbers
paraN :: (Eq a, Num a) => (a -> b -> b) -> b -> a -> b
paraN _ b 0 = b
paraN op b n = (n - 1) `op` (paraN op b (n - 1))

-- | Factorial defined using `paraN`
-- >>> factorial 5
-- 120
factorial :: (Num a, Eq a) => a -> a
factorial x = paraN op 1 x
  where
    n `op` m = (1 + n) * m

-- |
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insert2 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
insert2 :: forall a. (Ord a) => a -> List a -> List a
insert2 y = paraL f (wrap y)
  where
    f :: a -> (List a, List a) -> List a
    f x (xs, acc)
      | y < x = Cons y (Cons x xs)
      | otherwise = Cons x acc

-- ** Unfolds for lists

-- $
-- The dual of folding is unfolding
--
-- The Haskell standard library defines the follwoing function for generating
-- lists:
--
-- > unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- | An equivalent implementation of `unfoldr` for our `List` datatype
unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f u = case f u of
  Nothing -> Nil
  Just (x, v) -> Cons x (unfoldL' f v)

-- | `zip` in terms of `unfoldL'`
--
-- >>> zip (x, y)
-- Cons (1,4) (Cons (2,5) (Cons (3,6) Nil))
zip :: forall a b. (List a, List b) -> List (a, b)
zip = unfoldL' f
  where
    f :: (List a, List b) -> Maybe ((a, b), (List a, List b))
    f (Cons x xs, Cons y ys) = Just ((x, y), (xs, ys))
    f _ = Nothing

-- $
-- =Note:
-- Sometimes it is convenient to provide the single argument of `unfoldL'` as
-- three components: a predicate indicating when that argument should return
-- `Nothing`, and two functions yielding the two components of the pair when it
-- does not.

unfoldL ::
  -- | Predicate `p` determines when the seed should unfold the empty `List`
  (b -> Bool) ->
  -- | When @p == False@, `f` gives the head of the `List`
  (b -> a) ->
  -- | When @p == False@, `g` gives the seed from which to unfold the tail
  (b -> b) ->
  -- | The thing to unfold
  b ->
  -- | The unfolded value
  List a
unfoldL p f g b =
  if p b
    then Nil
    else Cons (f b) (unfoldL p f g (g b))

-- *** Exercise 3.6

-- $
-- Express `unfoldL` in terms of `unfoldL'`, and vice versa

unfoldL1 ::
  forall a b.
  (b -> Bool) ->
  (b -> a) ->
  (b -> b) ->
  b ->
  List a
unfoldL1 p f g = unfoldL' translator
  where
    translator :: b -> Maybe (a, b)
    translator x
      | p x = Nothing
      | otherwise = Just (f x, g x)

unfoldL2 :: forall a b. (b -> Maybe (a, b)) -> b -> List a
unfoldL2 h = unfoldL p f g
  where
    p :: b -> Bool
    p = isNothing . h

    f :: b -> a
    f x = case h x of
      Just (a, _) -> a
      Nothing -> error "something is wrong"

    g :: b -> b
    g x = case h x of
      Just (_, b) -> b
      Nothing -> error "something is wrong"

-- $
-- =Note:
-- Conversely, one could define a function `foldL'` taking a single argument of
-- type @Maybe (a, b) -> b@ in place of `foldL`'s two argument:

foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil = f Nothing
foldL' f (Cons x xs) = f (Just (x, foldL' f xs))

-- $
-- These primed versions make the duality between the fold and the unfold very
-- clear, although they sometimes be less convenient for programming with.

-- *** Exercise 3.8

-- $
-- Define `foldL'` in terms of `foldL`, and vice versa.
--
-- > foldL :: (a -> b -> b) -> b -> List a -> b

foldL1 :: forall a b. (Maybe (a, b) -> b) -> List a -> b
foldL1 f = foldL translator zero
  where
    translator :: a -> b -> b
    translator a b = f (Just (a, b))
    zero = f Nothing

foldL2 :: (a -> b -> b) -> b -> List a -> b
foldL2 f zero = foldL' (maybe zero (\(a, b) -> f a b))

-- *** Exercise 3.9

-- $
-- The adaptation of the single-argument fold and unfold to the multi-argument
-- interface is simplified by functions of the following types:

foldLargs :: forall a b. (a -> b -> b) -> b -> (Maybe (a, b) -> b)
foldLargs f zero = translator
  where
    translator :: Maybe (a, b) -> b
    translator (Just (a, b)) = f a b
    translator Nothing = zero

unfoldLargs ::
  forall a b.
  (b -> Bool) ->
  (b -> a) ->
  (b -> b) ->
  (b -> Maybe (a, b))
unfoldLargs p f g = translator
  where
    translator :: b -> Maybe (a, b)
    translator x
      | p x = Just (f x, g x)
      | otherwise = Nothing

-- $
-- One sorting algorithm expressible as a list unfold is /selection sort/, which
-- operates by at each step removing the minimum element of the list to be
-- sorted, but leaving the other elements in the same order.  We first define
-- the function /delmin/ to do this removal:

-- |
-- >>> delmin (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Just (1,Cons 6 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil)))))
delmin :: (Ord a) => List a -> Maybe (a, List a)
delmin Nil = Nothing
delmin xs = Just (y, deleteL y xs)
  where
    y = minimumL xs

-- | `minimumL` is the `List` equivalent of the standard library function
-- `minimum`
--
-- >>> minimumL (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- 1
minimumL :: (Ord a) => List a -> a
minimumL Nil = error "minimumL Nil"
minimumL (Cons x xs) = foldL min x xs

-- | `deleteL` is the `List` equivalent of the standard library function
-- `delete`
--
-- >>> deleteL 6 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))
deleteL :: (Eq a) => a -> List a -> List a
deleteL _ Nil = Nil
deleteL y (Cons x xs)
  | y == x = xs
  | otherwise = Cons x (deleteL y xs)

-- $
-- Then selection sort is straightforward to define:

-- |
-- >>> ssort (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
ssort :: (Ord a) => List a -> List a
ssort = unfoldL' delmin

-- *** Exercise 3.10

-- | A redefinition of `deleteL` in terms of `paraL`
--
-- >>> deleteL' 6 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))
-- >>> deleteL' 5 (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 6 (Cons 1 (Cons 2 (Cons 4 (Cons 3 Nil))))
deleteL' :: (Eq a) => a -> List a -> List a
deleteL' y = paraL f (wrap y)
  where
    f x (xs, acc)
      | y == x = xs
      | otherwise = Cons x acc

-- *** Exercise 3.11

-- | A redefinition of `delmin` in terms of `paraL`
--
-- >>> delmin' (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Just (1,Cons 6 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil)))))
-- >>> delmin' (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 Nil))))))
-- Just (7,Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 Nil)))))
delmin' :: forall a. (Ord a) => List a -> Maybe (a, List a)
delmin' = paraL f Nothing
  where
    f :: a -> (List a, Maybe (a, List a)) -> Maybe (a, List a)
    f x (xs, Nothing) = Just (x, xs)
    f x (xs, Just (m, acc))
      | x < m = Just (x, xs)
      | otherwise = Just (m, Cons x acc)

-- | `bubble` has the same type as `delmin`, but it does not preserve the
-- relative order of remaining list elements.  This means that it is possible to
-- define `bubble` as a fold.
bubble :: (Ord a) => List a -> Maybe (a, List a)
bubble = foldL step Nothing
  where
    step x Nothing = Just (x, Nil)
    step x (Just (y, ys))
      | x < y = Just (x, Cons y ys)
      | otherwise = Just (y, Cons x ys)

-- |
-- >>> bsort (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
bsort :: (Ord a) => List a -> List a
bsort = unfoldL' bubble

-- *** Exercise 3.12

-- | An alternate version of `bubble` that returns a `List` with the minimum
-- element "bubbled" to the top
bubble' :: (Ord a) => List a -> List a
bubble' = foldL f Nil
  where
    f x Nil = Cons x Nil
    f x (Cons m xs)
      | x < m = Cons x (Cons m xs)
      | otherwise = Cons m (Cons x xs)

-- |
-- >>> bsort' (Cons 6 (Cons 1 (Cons 5 (Cons 2 (Cons 4 (Cons 3 Nil))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))
bsort' :: (Ord a) => List a -> List a
bsort' = unfoldL' b
  where
    b xs = case bubble' xs of
      (Cons y ys) -> Just (y, ys)
      Nil -> Nothing

-- *** Exercise 3.13

-- |
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insertWithUnfold 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
insertWithUnfold :: (Ord a) => a -> List a -> List a
insertWithUnfold x xs = unfoldL' inserter (Just x, xs)
  where
    inserter (Just i, Cons y ys)
      | i < y = Just (i, (Nothing, Cons y ys))
      | otherwise = Just (y, (Just i, ys))
    inserter (Nothing, Cons y ys) = Just (y, (Nothing, ys))
    inserter (Just i, Nil) = Just (i, (Nothing, Nil))
    inserter (Nothing, Nil) = Nothing

-- *** Exercise 3.14

-- $
-- `insertWithUnfold` is a bit unsatisfactory, because once the correct position
-- is found at which to insert the element, the remainder of the list must still
-- be copied item by item.
--
-- The direct recursive definition did not have this problem: one branch shares
-- the remainder of the original list without making a recursive call.
--
-- This general pattern can be captured as another recursion operator, known as
-- an /apomorphism/.

-- | A function for apomorphisms.  For non-empty lists, the generation function
-- `f` yields `Either` a new seed, on which a recursive call is made, or a
-- complete list, which is used directly.
apoL' :: (b -> Maybe (a, Either b (List a))) -> b -> List a
apoL' f u = case f u of
  Nothing -> Nil
  Just (x, Left v) -> Cons x (apoL' f v)
  Just (x, Right xs) -> Cons x xs

-- | `insert` as an instance of `apoL'`.
--
-- >>> let z = Cons 1 (Cons 2 (Cons 4 Nil))
-- >>> insertWithApo 3 z
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
insertWithApo :: forall a. (Ord a) => a -> List a -> List a
insertWithApo x xs = apoL' apper xs
  where
    apper :: (List a -> Maybe (a, Either (List a) (List a)))
    apper (Cons y ys)
      | y < x = Just (y, Left ys)
      | otherwise = Just (x, Right (Cons y ys))
    apper Nil = Nothing

-- ** Hylomorphisms

-- $
-- Unfolds generate data structures, and folds consume them; it is natural to
-- compose these two operations.
--
-- The pattern of computation consisting of an unfold followed by a fold is a
-- fairly common one.  Such compositions are called /hylomorphisms/.
--
-- A simple example of a hylomorphism is given by the factorial function:

-- |
-- >>> fact 5
-- 120
fact :: Integer -> Integer
fact = foldL (*) 1 . unfoldL (== 0) id pred

-- $
-- More elaborate examples of hylomorphisms (on trees) are provided by
-- traditional compilers, which may be thought of as constructing an abstract
-- syntax tree (unfolding to the tree type) from which to generate code (folding
-- the abstract syntax tree).

hyloL ::
  (a -> c -> c) ->
  c ->
  (b -> Bool) ->
  (b -> a) ->
  (b -> b) ->
  b ->
  c
hyloL f e p g h = foldL f e . unfoldL p g h

-- |
-- >>> fact' 5
-- 120
fact' :: Integer -> Integer
fact' = hyloL (*) 1 (== 0) id pred

hyloLFused ::
  (a -> c -> c) ->
  c ->
  (b -> Bool) ->
  (b -> a) ->
  (b -> b) ->
  b ->
  c
hyloLFused f e p g h b =
  if p b
    then e
    else f (g b) (hyloLFused f e p g h (h b))

-- |
-- >>> factFused 5
-- 120
factFused :: Integer -> Integer
factFused = hyloL (*) 1 (== 0) id pred

-- *** Exercise 3.15

type Binary = List Bool

decimalStringToBinary :: String -> Binary
decimalStringToBinary = undefined

-- * Origami by numbers: loops

data Nat = Zero | Succ Nat deriving (Show)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = Succ (intToNat (x - 1))

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

-- ** Folds for naturals

foldN :: a -> (a -> a) -> Nat -> a
foldN z _ Zero = z
foldN z s (Succ n) = s (foldN z s n)

-- $
-- If we reverse the order of the three arguments, we see that `foldN` is in
-- fact and old friend...

-- | A higher-order function that applies a given function of type @a -> a@ a
-- given number of times
iter :: Nat -> (a -> a) -> a -> a
iter n f x = foldN x f n

-- *** Exercise 3.16

-- | The single-argument version of foldN
foldN' :: (Maybe a -> a) -> Nat -> a
foldN' f Zero = f Nothing
foldN' f (Succ n) = f (Just (foldN' f n))

-- | `foldN'` in terms of `foldN`
foldN1 :: (Maybe a -> a) -> Nat -> a
foldN1 f ns = foldN (f Nothing) (f . Just) ns

-- | `foldN` in terms of `foldN'`
foldN2 :: a -> (a -> a) -> Nat -> a
foldN2 z s = foldN' (maybe z s)

-- *** Exercise 3.18

-- |
-- >>> addN (Succ Zero) (Succ (Succ Zero))
-- Succ (Succ (Succ Zero))
addN :: Nat -> Nat -> Nat
addN m = foldN m Succ

-- |
-- >>> mulN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
mulN :: Nat -> Nat -> Nat
mulN m = foldN Zero (addN m)

-- |
-- >>> powN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
powN :: Nat -> Nat -> Nat
powN m = foldN (Succ Zero) (mulN m)

-- *** Exercise 3.19

predN :: Nat -> Maybe Nat
predN Zero = Nothing
predN (Succ n) = Just n

-- | `predN` in terms of `foldN`
--
-- >>> predN' (Succ (Succ (Succ Zero)))
-- Just (Succ (Succ Zero))
-- >>> predN' Zero
-- Nothing
predN' :: Nat -> Maybe Nat
predN' = foldN Nothing f
  where
    f Nothing = Just Zero
    f (Just n) = Just (Succ n)

-- *** Exercise 3.20

-- |
-- >>> subN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- Just (Succ Zero)
subN :: Nat -> Nat -> Maybe Nat
subN m = foldN (Just m) ((=<<) predN)

-- |
-- >>> eqN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- False
-- >>> eqN (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))
-- True
eqN :: Nat -> Nat -> Bool
eqN (Succ m) (Succ n) = eqN m n
eqN (Succ _) Zero = False
eqN Zero (Succ _) = False
eqN Zero Zero = True

-- |
-- >>> lessN (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- False
-- >>> lessN (Succ (Succ Zero)) (Succ (Succ Zero))
-- False
-- >>> lessN (Succ Zero) (Succ (Succ (Succ Zero)))
-- True
lessN :: Nat -> Nat -> Bool
lessN (Succ m) (Succ n) = lessN m n
lessN (Succ _) Zero = False
lessN Zero (Succ _) = True
lessN Zero Zero = False

-- ** Unfolds for naturals

unfoldN' :: (a -> Maybe a) -> a -> Nat
unfoldN' f x = case f x of
  Nothing -> Zero
  Just y -> Succ (unfoldN' f y)

-- | A version of `unfoldN'` which splits the single argument into simpler
-- components.
--
-- Here we find another old friend: this is the minimisation function form
-- recursive function theory, which takes a predicate @p@, a function @f@ and a
-- value @x@, and computer the least number @n@ such that @p (iter n f x)@
-- holds.
unfoldN :: (a -> Bool) -> (a -> a) -> a -> Nat
unfoldN p f x = if p x then Zero else Succ (unfoldN p f (f x))

-- *** Exercise 3.21

unfoldN1 :: forall a. (a -> Maybe a) -> a -> Nat
unfoldN1 f = unfoldN p g
  where
    p :: a -> Bool
    p = isNothing . f

    g :: a -> a
    g x = case f x of
      Just a -> a
      Nothing -> error "something is wrong"

unfoldN2 :: forall a. (a -> Bool) -> (a -> a) -> a -> Nat
unfoldN2 p f = unfoldN' translator
  where
    translator :: a -> Maybe a
    translator x
      | p x = Nothing
      | otherwise = Just (f x)

-- *** Exercise 3.23

-- |
-- >>> let eight = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))
-- >>> let two   = Succ (Succ Zero)
-- >>> divN eight two
-- Succ (Succ (Succ (Succ Zero)))
divN :: Nat -> Nat -> Nat
divN m n = unfoldN' f m
  where
    f :: Nat -> Maybe Nat
    f x@(Succ _) = subN x n
    f Zero = Nothing

-- *** Exercise 3.24

logN :: Nat -> Nat
logN = undefined

-- ** Beyond primtive recursion

untilN :: (a -> Bool) -> (a -> a) -> a -> a
untilN p f x = foldN x f (unfoldN p f x)

-- $
-- At first sight, this appears somewhat different than the prelude's
-- definition:
--
-- > until :: (a -> Bool) -> (a -> a) -> a -> a
-- > until p f x = if p x then x else until p f (f x)
--
-- Our definition first computes the number of iterations that will be required
-- and the iterates the loop body that many times; the prelude's definition uses
-- but a single loop.  Nevertheless, the prelude's definition arises by
-- deforesting the number of iterations - at least for strict @f@.

-- *** Exercise 3.25

hyloN' :: (Maybe a -> a) -> (a -> Maybe a) -> a -> a
hyloN' f g = foldN' f . unfoldN' g

hyloN :: (Maybe a -> a) -> (a -> Maybe a) -> a -> a
hyloN f g x = case g x of
  Nothing -> x
  Just y -> hyloN f g (f (Just y))

-- * Origami with trees: traversals

data Rose a = Node a (Forest a) deriving (Show)

type Forest a = List (Rose a)

-- ** Folds for trees and forests

-- $
-- Since the types of trees and forests are mutually recursive, it seems
-- "sweetly reasonable" that the folds too should be mutually recursive.

-- |
-- >>> let t = Node 42 (Cons (Node 43 Nil) (Cons (Node 44 Nil) (Cons (Node 45 Nil) Nil)))
-- >>> let plus1 = \ a g -> Node (a + 1) g
-- >>> foldR plus1 id t
-- Node 43 (Cons (Node 44 Nil) (Cons (Node 45 Nil) (Cons (Node 46 Nil) Nil)))
foldR :: (a -> g -> b) -> (List b -> g) -> Rose a -> b
foldR f g (Node a ts) = f a (foldF f g ts)

foldF :: (a -> g -> b) -> (List b -> g) -> Forest a -> g
foldF f g ts = g (mapL (foldR f g) ts)

-- *** Exercise 3.29

-- |
-- >>> let t = Node 42 (Cons (Node 43 Nil) (Cons (Node 44 Nil) (Cons (Node 45 Nil) Nil)))
-- >>> let plus1 = \ a g -> Node (a + 1) g
-- >>> foldRose plus1 t
-- Node 43 (Cons (Node 44 Nil) (Cons (Node 45 Nil) (Cons (Node 46 Nil) Nil)))
foldRose :: (a -> List b -> b) -> Rose a -> b
foldRose f (Node a ts) = f a (mapL (foldRose f) ts)

-- | `foldRose` defined in terms of `foldR` and `foldF`
--
-- >>> let t = Node 42 (Cons (Node 43 Nil) (Cons (Node 44 Nil) (Cons (Node 45 Nil) Nil)))
-- >>> let plus1 = \ a g -> Node (a + 1) g
-- >>> foldRose1 plus1 t
-- Node 43 (Cons (Node 44 Nil) (Cons (Node 45 Nil) (Cons (Node 46 Nil) Nil)))
foldRose1 :: (a -> List b -> b) -> Rose a -> b
foldRose1 h = foldR h id

-- | `foldR` defined in terms of `foldRose`
--
-- >>> let t = Node 42 (Cons (Node 43 Nil) (Cons (Node 44 Nil) (Cons (Node 45 Nil) Nil)))
-- >>> let plus1 = \ a g -> Node (a + 1) g
-- >>> foldR1 plus1 id t
-- Node 43 (Cons (Node 44 Nil) (Cons (Node 45 Nil) (Cons (Node 46 Nil) Nil)))
foldR1 :: forall a g b. (a -> g -> b) -> (List b -> g) -> Rose a -> b
foldR1 f g = foldRose h
  where
    h :: a -> List b -> b
    h a bs = f a (g bs)

-- ** Unfolds for trees and forests

-- $
-- Similarly, there is a mutually recursive pair of unfold functions, both
-- taking the same functional arguments.  In this case, the arguments generate
-- from a seed a root label and a list of new seeds; the two unfolds frow from a
-- seed a tree and a forest respectively.

unfoldR :: (b -> a) -> (b -> List b) -> b -> Rose a
unfoldR f g x = Node (f x) (unfoldF f g x)

unfoldF :: (b -> a) -> (b -> List b) -> b -> Forest a
unfoldF f g x = mapL (unfoldR f g) (g x)

-- $
-- For convenience in what follows, we define separate destructors for the root
-- and the list of a children of a tree.

root :: Rose a -> a
root (Node a _) = a

kids :: Rose a -> Forest a
kids (Node _ ts) = ts

-- ** Depth-first traversal

-- $
-- Because folds on trees and on forests are mutually recursive with the same
-- functions as arguments, a commmon idiom when using them is to define the two
-- simultaneously as a pair of functions.
--
-- For example, consider performing the depth-first traversal of a tree or a
-- forest.  The traversal of a tree is one item longer than the traversal of its
-- children; the traversal of a forest is obtained by concatenating the
-- traversals of its trees.

dft :: Rose a -> List a
dff :: Forest a -> List a
(dft, dff) = (foldR f g, foldF f g)
  where
    f = Cons
    g = concatL

-- ** Breadth-first traversal

-- $
-- Depth-first traversal is in a sense the natural traversal on trees; in
-- contrast, breadth-first traversal goes "against the grain".  We cannot define
-- breadth-first traversal as a fold in the same way as we did for depth-first
-- traversal, because it is not compositional - it is not possible to construct
-- the traversal of a forest from the traversals of its trees.
--
-- The usual implementation of breadth-first traversal in an imperative langauge
-- involves queues.  Queueing does not come naturally to functional programmers,
-- although Okasaki has done a lot towards rectifying that situation.  In
-- contrast, depth-first traversal is based on a stack, and stacks come for free
-- with recursive programs.

-- ** Level-order traversal

-- $
-- However, one can make some progress: one can compute the level-order
-- traversal compositionally.  This yields not just a list, but a list of lists
-- of elements, with one list for each level of the tree.

levelt :: Rose a -> List (List a)
levelf :: Forest a -> List (List a)
(levelt, levelf) = (foldR f g, foldF f g)
  where
    f x xss = Cons (wrap x) xss
    g = foldL (lzw appendL) Nil

-- $
-- The level-order traversal of a forest is obtained by gluing together the
-- traversals of its trees; two lists of lists may be glued appropriately by
-- concatenating corresponding elements.  This gluing is performed above by the
-- function @lzw appendL@ (called @combine@ in IPFH).  The identifier @lzw@ here
-- stands for "long zip with"; it is like the @zipWith@ function from the
-- standard prelude, but returns a list whose length is the length of the longer
-- argument, as opposed to that of the shorter one.

lzw :: (a -> a -> a) -> List a -> List a -> List a
lzw _ Nil ys = ys
lzw _ xs Nil = xs
lzw f (Cons x xs) (Cons y ys) = Cons (f x y) (lzw f xs ys)

-- *** Exercise 3.32

-- | `lzw` as an `unfold`
lzw' :: forall a. (a -> a -> a) -> List a -> List a -> List a
lzw' f ls rs = unfoldL' ufer (ls, rs)
  where
    ufer :: (List a, List a) -> Maybe (a, (List a, List a))
    ufer (Cons x xs, Cons y ys) = Just (f x y, (xs, ys))
    ufer (Nil, Cons y ys) = Just (y, (Nil, ys))
    ufer (Cons x xs, Nil) = Just (x, (xs, Nil))
    ufer (Nil, Nil) = Nothing

-- *** Exercise 3.33

-- | `lzw` in terms of `apoL'`
lzwApo :: forall a. (a -> a -> a) -> List a -> List a -> List a
lzwApo f ls rs = apoL' apoer (ls, rs)
  where
    apoer :: (List a, List a) -> Maybe (a, Either (List a, List a) (List a))
    apoer (Cons x xs, Cons y ys) = Just (f x y, Left (xs, ys))
    apoer (Nil, Cons y ys) = Just (y, Right ys)
    apoer (Cons x xs, Nil) = Just (x, Right xs)
    apoer (Nil, Nil) = Nothing

-- $
-- Of course, having obtained the level-order traversal of a tree or a forest,
-- it is straightforward to obtain the breadth-first traversal: simply
-- concatenate the levels.

bft :: Rose a -> List a
bft = concatL . levelt

bff :: Forest a -> List a
bff = concatL . levelf

-- ** Accumulating parameters

-- $
-- The native definitions of `levelt` and `levelf` are inefficient, because of
-- the repeated list concatenations.  The standard accumulating parameter
-- technique can be used here.  In each case, the accumulating parameter is a
-- list of lists; the specifications of the two new functions are:

levelt' :: Rose a -> List (List a) -> List (List a)
levelt' t = lzw appendL (levelt t)

levelf' :: Forest a -> List (List a) -> List (List a)
levelf' ts = lzw appendL (levelf ts)

-- ** Level-order traversal as an unfold

-- $
-- ...

-- * Other sorts of origami

-- ** Shell sort

-- $
-- Shell sort improves on insertion sort by allowing exchanges initially between
-- distant elements.

-- *** Exercise 3.41

-- | `repeat` for `List`s
repeat :: a -> List a
repeat x = Cons x (repeat x)

-- | Zippy application
zapp :: List (a -> b) -> List a -> List b
zapp (Cons f fs) (Cons x xs) = Cons (f x) (zapp fs xs)
zapp _ _ = Nil

-- | `List` transposition
--
-- I'M TIRED OF DEFINING EVERYTHING IN TERMS OF FOLDS AND UNFOLDS
--
-- >>> let x = Cons 1 (Cons 2 (Cons 3 Nil))
-- >>> let y = Cons 4 (Cons 5 (Cons 6 Nil))
-- >>> let z = Cons 7 (Cons 8 (Cons 9 Nil))
-- >>> let m = Cons x (Cons y (Cons z Nil))
-- >>> trans m
-- Cons (Cons 1 (Cons 4 (Cons 7 Nil))) (Cons (Cons 2 (Cons 5 (Cons 8 Nil))) (Cons (Cons 3 (Cons 6 (Cons 9 Nil))) Nil))
trans :: forall a. List (List a) -> List (List a)
trans Nil = repeat Nil
trans (Cons x xs) = repeat Cons `zapp` x `zapp` (trans xs)

ravel :: List (List a) -> List a
ravel = concatL . trans

-- *** Exercise 3.42

-- |
-- >>> takeL (Succ (Succ Zero)) x
-- Cons 1 (Cons 2 Nil)
takeL :: Nat -> List a -> List a
takeL m as = unfoldL' f (m, as)
  where
    f (Zero, _) = Nothing
    f (Succ _, Nil) = Nothing
    f (Succ n, Cons x xs) = Just (x, (n, xs))

-- |
-- >>> dropL (Succ (Succ Zero)) x
-- Cons 3 Nil
dropL :: Nat -> List a -> List a
dropL m as = foldN as f m
  where
    f (Cons _ xs) = xs
    f Nil = Nil

-- |
-- >>> let x = Cons 1 (Cons 2 (Cons 3 Nil))
-- >>> let y = Cons 4 (Cons 5 (Cons 6 Nil))
-- >>> let z = Cons 7 (Cons 8 (Cons 9 Nil))
-- >>> let m = appendL x (appendL y z)
-- >>> unravel (Succ (Succ (Succ Zero))) m
-- Cons (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons (Cons 4 (Cons 5 (Cons 6 Nil))) (Cons (Cons 7 (Cons 8 (Cons 9 Nil))) Nil))
-- >>> ravel (unravel (Succ (Succ (Succ Zero))) m)
-- Cons 1 (Cons 4 (Cons 7 (Cons 2 (Cons 5 (Cons 8 (Cons 3 (Cons 6 (Cons 9 Nil))))))))
unravel :: forall a. Nat -> List a -> List (List a)
unravel _ Nil = Nil
unravel m xs = Cons (takeL m xs) (unravel m (dropL m xs))

-- *** Exercise 3.43

-- |
-- >>> hsort (Succ (Succ (Succ Zero))) (Cons 9 (Cons 6 (Cons 1 (Cons 8 (Cons 5 (Cons 2 (Cons 7 (Cons 4 (Cons 3 Nil)))))))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 6 (Cons 5 (Cons 4 (Cons 9 (Cons 8 (Cons 7 Nil))))))))
hsort :: (Ord a) => Nat -> List a -> List a
hsort n = ravel . (mapL isort) . (unravel n)

shell :: (Ord a) => List Nat -> List a -> List a
shell Nil xs = xs
shell (Cons n ns) xs = shell ns (hsort n xs)

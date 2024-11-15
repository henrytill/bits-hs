{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Bits.ALaCarte
-- Description : Data types a la carte
--
-- Code from Wouter Swierstra's
-- /Data types a la carte/.
module Bits.ALaCarte where

import Prelude hiding (getChar, putChar, readFile, writeFile)
import qualified Prelude

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators

-- * 2 Fixing the expression problem

data Expr f = In (f (Expr f))

data Val e = Val Int

type IntExpr = Expr Val

data Add e = Add e e

type AddExpr = Expr Add

-- | The coproduct of two signatures.  Used for combining expressions.
infixr 6 :+:

data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

-- * 3 Evaluation

instance Functor Val where
  fmap _ (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: (Functor f) => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class (Functor f) => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

-- |
-- >>> eval addExample
-- 1337
eval :: (Eval f) => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- * 4 Automating injections

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  prj = Just

instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj (Inl x) = Just x
  prj (Inr _) = Nothing

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj (Inl _) = Nothing
  prj (Inr y) = prj y

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 `add`

add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add x y = inject (Add x y)

-- $
-- >>> let x :: Expr (Add :+: Val) = val 30000 `add` val 1330 `add` val 7
-- >>> eval x
-- 31337

inVal :: Int -> Expr (Val :+: Val)
inVal i = inject (Val i)

-- * 5 Examples

data Mul x = Mul x x

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

infixl 7 `mul`

mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f
mul x y = inject (Mul x y)

-- $
-- >>> let x :: Expr (Val :+: Add :+: Mul) = val 80 `mul` val 5 `add` val 4
-- >>> eval x
-- 404
-- >>> let y :: Expr (Val :+: Mul) = val 6 `mul` val 7
-- >>> eval y
-- 42

-- | Renders an expression to a string.
class Render f where
  render :: (Render g) => f (Expr g) -> String

-- | Pretty-prints an expression.
pretty :: (Render f) => Expr f -> String
pretty (In t) = render t

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"

instance Render Mul where
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y

-- $
-- >>> let x :: Expr (Val :+: Add :+: Mul) = val 80 `mul` val 5 `add` val 4
-- >>> pretty x
-- "((80 * 5) + 4)"

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

-- |
-- Applies the distributive law on the outermost constructors of an expression.
--
-- >>> let x :: Expr (Val :+: Add :+: Mul) = val 80 `mul` (val 5 `add` val 4)
-- >>> let Just y = distr x
-- >>> pretty y
-- "((80 * 5) + (80 * 4))"
distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = do
  Mul a b <- match t
  Add c d <- match b
  return (a `mul` c `add` a `mul` d)

-- * 6 Monads for free

data Term f a
  = Pure a
  | Impure (f (Term f a))

instance (Functor f) => Functor (Term f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)

instance (Functor f) => Applicative (Term f) where
  pure = Pure
  Pure f <*> Pure x = Pure (f x)
  Pure f <*> Impure t = Impure (fmap (fmap f) t)
  Impure t <*> x = Impure (fmap (<*> x) t)

instance (Functor f) => Monad (Term f) where
  return = pure
  Pure x >>= f = f x
  Impure t >>= f = Impure (fmap (>>= f) t)

data Zero a deriving (Functor)

data One a = One deriving (Functor, Show)

data Const e a = Const e deriving (Functor, Show)

-- |
-- >>> let (Pure x) = identExample
-- >>> x
-- 12
identExample :: Term Zero Int
identExample = (*) <$> pure 3 <*> pure 4

-- |
-- >>> let (Impure x) = maybeExample
-- >>> x
-- One
maybeExample :: Term One Int
maybeExample = (*) <$> pure 3 <*> Impure One

-- |
-- >>> let (Impure x) = errorExample
-- >>> x
-- Const "quux"
errorExample :: Term (Const String) Int
errorExample = (*) <$> pure 3 <*> Impure (Const "quux")

data Incr t = Incr Int t deriving (Functor)

data Recall t = Recall (Int -> t) deriving (Functor)

minject :: (g :<: f) => g (Term f a) -> Term f a
minject = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr i = minject (Incr i (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = minject (Recall Pure)

tick :: Term (Recall :+: Incr) Int
tick = do
  y <- recall
  incr 1
  return y

foldTerm :: (Functor f) => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pur _ (Pure x) = pur x
foldTerm pur imp (Impure t) = imp (fmap (foldTerm pur imp) t)

newtype Mem = Mem Int
  deriving (Show)

class (Functor f) => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run Incr where
  runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
  runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

run :: (Run f) => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra

-- $
-- >>> run tick (Mem 4)
-- (4,Mem 5)

-- * 7 Applications

data Teletype a
  = GetChar (Char -> a)
  | PutChar Char a
  deriving (Functor)

data FileSystem a
  = ReadFile FilePath (String -> a)
  | WriteFile FilePath String a
  deriving (Functor)

exec :: (Exec f) => Term f a -> IO a
exec = foldTerm return execAlgebra

class (Functor f) => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra (GetChar f) = Prelude.getChar >>= f
  execAlgebra (PutChar c io) = Prelude.putChar c >> io

instance Exec FileSystem where
  execAlgebra (ReadFile fp g) = Prelude.readFile fp >>= g
  execAlgebra (WriteFile fp str x) = Prelude.writeFile fp str >> x

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra (Inl x) = execAlgebra x
  execAlgebra (Inr y) = execAlgebra y

getChar :: (Teletype :<: f) => Term f Char
getChar = minject (GetChar Pure)

putChar :: (Teletype :<: f) => Char -> Term f ()
putChar c = minject (PutChar c (Pure ()))

readFile :: (FileSystem :<: f) => FilePath -> Term f String
readFile fp = minject (ReadFile fp Pure)

writeFile :: (FileSystem :<: f) => FilePath -> String -> Term f ()
writeFile fp str = minject (WriteFile fp str (Pure ()))

cat :: FilePath -> Term (Teletype :+: FileSystem) ()
cat fp = do
  contents <- readFile fp
  mapM_ putChar contents
  return ()

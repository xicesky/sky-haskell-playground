
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{- | Compositional data types
See http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.6303&rep=rep1&type=pdf
    http://bahr.io/pubs/files/bahr11wgp-slides%20(full).pdf
And https://hackage.haskell.org/package/compdata
-}

module Sky.Learn.Compositional where

----------------------------------------------------------------------------------------------------
-- Essentially Data.Fix

-- Initial f-Algebra
data Term f = Term (f (Term f))

instance Show (f (Term f)) => Show (Term f) where
    show (Term x) = "(" ++ show x ++ ")"
  
instance Eq (f (Term f)) => Eq (Term f) where
    (Term a) == (Term b) = a == b

----------------------------------------------------------------------------------------------------

data (f :+: g) e
    = Inl (f e)
    | Inr (g e)

----------------------------------------------------------------------------------------------------

data Op e
    = Mult e e
    | Fst e
    deriving (Eq, Show)

data Val e
    = Const Int
    | Pair e e
    deriving (Eq, Show)

type ExpS = Val :+: Op

type Exp = Term ExpS
type Value = Term Val

const' :: Int -> Exp
const' x    = Term $ Inl $ Const x
pair' :: Exp -> Exp -> Exp
pair' x y   = Term $ Inl $ Pair x y
mult' :: Exp -> Exp -> Exp
mult' x y   = Term $ Inr $ Mult x y
fst' :: Exp -> Exp
fst' x      = Term $ Inr $ Fst x

vconst' :: Int -> Value
vconst' x   = Term $ Const x
vpair' :: Value -> Value -> Value
vpair' x y  = Term $ Pair x y

----------------------------------------------------------------------------------------------------
-- Algebra

eval :: Exp -> Value
eval (Term (Inl (Const n)))     = vconst' n
eval (Term (Inl (Pair x y)))    = vpair' (eval x) (eval y)
eval (Term (Inr (Mult x y)))    = let
    (Term (Const m))   = eval x
    (Term (Const n))   = eval y
    in vconst' (m * n)
eval (Term (Inr (Fst p)))       = let
    (Term (Pair x y))  = eval p
    in x

----------------------------------------------------------------------------------------------------
-- Example

expExample :: Exp
expExample = (fst' (pair' (const' 2) (const' 3))) `mult'` (const' 5)

main :: IO ()
main = print $ eval expExample

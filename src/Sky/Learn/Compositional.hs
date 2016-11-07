
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
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

data ExpS r
    = Const Int
    | Pair  r r
    | Mult  r r
    | Fst   r
    deriving (Eq, Show)

data ValueS r
    = VConst Int
    | VPair r r
    deriving (Eq, Show)

type Exp = Term ExpS
type Value = Term ValueS

const' x    = Term $ Const x
pair' x y   = Term $ Pair x y
mult' x y   = Term $ Mult x y
fst' x      = Term $ Fst x

vconst' x   = Term $ VConst x
vpair' x y  = Term $ VPair x y

----------------------------------------------------------------------------------------------------
-- Algebra

eval :: Exp -> Value
eval (Term (Const n))   = vconst' n
eval (Term (Pair x y))  = vpair' (eval x) (eval y)
eval (Term (Mult x y))  = let
    (Term (VConst m))   = eval x
    (Term (VConst n))   = eval y
    in vconst' (m * n)
eval (Term (Fst p))     = let
    (Term (VPair x y))  = eval p
    in x

----------------------------------------------------------------------------------------------------
-- Example

expExample :: Exp
expExample = (fst' (pair' (const' 2) (const' 3))) `mult'` (const' 5)

main :: IO ()
main = print $ eval expExample


{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{- | Compositional data types
See http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.6303&rep=rep1&type=pdf
    http://bahr.io/pubs/files/bahr11wgp-slides%20(full).pdf
And https://hackage.haskell.org/package/compdata
-}

module Sky.Learn.Compositional where

----------------------------------------------------------------------------------------------------

data Exp
    = Const Int
    | Pair  Exp Exp
    | Mult  Exp Exp
    | Fst   Exp
    deriving (Eq, Show)

data Value
    = VConst Int
    | VPair Value Value
    deriving (Eq, Show)

----------------------------------------------------------------------------------------------------
-- Algebra

eval :: Exp -> Value
eval (Const n)  = VConst n
eval (Pair x y) = VPair (eval x) (eval y)
eval (Mult x y) = let       -- Problem: Non-exhaustive pattern -> GADTs?
    VConst m    = eval x
    VConst n    = eval y
    in VConst (m * n)
eval (Fst p)    = let
    VPair x y   = eval p
    in x

----------------------------------------------------------------------------------------------------
-- Example

expExample :: Exp
expExample = (Fst (Pair (Const 2) (Const 3))) `Mult` (Const 5)

main :: IO ()
main = print $ eval expExample

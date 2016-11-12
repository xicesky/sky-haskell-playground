
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TypeOperators          #-}

module Sky.Compositional.Demo where

import Sky.Compositional.Algebra

----------------------------------------------------------------------------------------------------

data Op e
    = Mult e e
    | Fst e
    deriving (Eq, Show, Functor)

iFst :: (Op :<: f) => Term f -> Term f
iFst x = inject (Fst x)
iMult :: (Op :<: f) => Term f -> Term f -> Term f
iMult x y = inject (Mult x y)

data Val e
    = Const Int
    | Pair e e
    deriving (Eq, Show, Functor)

iConst :: (Val :<: f) => Int -> Term f
iConst x = inject (Const x)
iPair :: (Val :<: f) => Term f -> Term f -> Term f
iPair x y = inject (Pair x y)

type ExpS = Val :+: Op

type Exp = Term ExpS
type Value = Term Val

const' :: Int -> Exp
const'      = iConst
pair' :: Exp -> Exp -> Exp
pair'       = iPair     

mult' :: Exp -> Exp -> Exp
mult'       = iMult
fst' :: Exp -> Exp
fst'        = iFst

vconst' :: Int -> Value
vconst'     = iConst
vpair' :: Value -> Value -> Value
vpair'      = iPair

----------------------------------------------------------------------------------------------------
-- Algebra

class Eval f where
    evalAlg :: f (Term Val) -> Term Val

instance Eval Val where
    evalAlg :: Val (Term Val) -> Term Val
    evalAlg = inject

instance Eval Op where
    evalAlg :: Op (Term Val) -> Term Val
    evalAlg (Mult x y) = case (unTerm x, unTerm y) of
        (Const m, Const n)  -> iConst (m * n)
        _                   -> error "Argh"
    evalAlg (Fst p) = case (unTerm p) of
        (Pair x y)  -> x
        _           -> error "Blargh"

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlg :: (f :+: g) (Term Val) -> Term Val
    evalAlg (Inl a) = evalAlg a
    evalAlg (Inr a) = evalAlg a

eval :: Exp -> Value
eval = cata evalAlg

----------------------------------------------------------------------------------------------------
-- Example

expExample :: Exp
expExample = (fst' (pair' (const' 2) (const' 3))) `mult'` (const' 5)

main :: IO ()
main = print $ eval expExample

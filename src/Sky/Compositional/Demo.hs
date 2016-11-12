
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
expExample = iFst (iPair (iConst 2) (iConst 3)) `iMult` iConst 5

main :: IO ()
main = print $ eval expExample


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
    | Snd e
    deriving (Eq, Show, Functor)

iFst :: (Op :<: f) => Term f -> Term f
iFst x = inject (Fst x)
iSnd :: (Op :<: f) => Term f -> Term f
iSnd x = inject (Snd x)
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

data Sug e
    = Neg e
    | Swap e
    deriving (Eq, Show, Functor)

iNeg :: (Sug :<: f) => Term f -> Term f
iNeg = inject . Neg
iSwap :: (Sug :<: f) => Term f -> Term f
iSwap = inject . Swap

type Exp = Val :+: Op
type Exp' = Sug :+: Exp

type VTerm = Term Val
type ETerm = Term Exp
type ETerm' = Term Exp'

----------------------------------------------------------------------------------------------------
-- Algebra

class Eval f where
    evalAlg :: f (Term Val) -> Term Val

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlg :: (f :+: g) (Term Val) -> Term Val
    evalAlg (Inl a) = evalAlg a
    evalAlg (Inr a) = evalAlg a

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
    evalAlg (Snd p) = case (unTerm p) of
        (Pair x y)  -> y
        _           -> error "Blargh"

eval :: ETerm -> VTerm
eval = cata evalAlg

class Desug f where
    desugAlg :: f (Term Exp) -> Term Exp

instance (Desug f, Desug g) => Desug (f :+: g) where
    desugAlg :: (f :+: g) (Term Exp) -> Term Exp
    desugAlg (Inl a) = desugAlg a
    desugAlg (Inr a) = desugAlg a

instance Desug Val where
    desugAlg :: Val (Term Exp) -> Term Exp
    desugAlg = inject

instance Desug Op where
    desugAlg :: Op (Term Exp) -> Term Exp
    desugAlg = inject

instance Desug Sug where
    desugAlg :: Sug (Term Exp) -> Term Exp
    desugAlg (Neg x) = iConst (-1) `iMult` x
    desugAlg (Swap x) = iPair (iSnd x) (iFst x)

desug :: ETerm' -> ETerm
desug = cata desugAlg

----------------------------------------------------------------------------------------------------
-- Example

expExample :: ETerm
expExample = iFst (iPair (iConst 2) (iConst 3)) `iMult` iConst 5

sugExample :: ETerm'
sugExample = iFst (iSwap (iPair (iConst 3) (iConst 2))) `iMult` iConst 5

main :: IO ()
main = print $ (eval . desug) sugExample


{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

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

-- Unsafe (!!)
projConst :: (Val :<: f) => Term f -> Int
projConst v = case project v of Just (Const n) -> n
projPair :: (Val :<: f) => Term f -> (Term f, Term f)
projPair v = case project v of Just (Pair x y) -> (x, y)

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

class Eval f v where
    evalAlg :: f v -> v

instance (Eval f v, Eval g v) => Eval (f :+: g) v where
    evalAlg :: (f :+: g) v -> v
    evalAlg (Inl a) = evalAlg a
    evalAlg (Inr a) = evalAlg a

instance (Val :<: v) => Eval Val (Term v) where
    evalAlg :: Val (Term v) -> Term v
    evalAlg = inject

instance (Val :<: v) => Eval Op (Term v) where
    evalAlg :: Op (Term v) -> Term v
    evalAlg (Mult x y) = iConst $ projConst x * projConst y
    evalAlg (Fst p) = fst $ projPair p
    evalAlg (Snd p) = snd $ projPair p

eval :: ETerm -> VTerm
eval = cata evalAlg

class Desug f g where
    desugAlg :: f g -> g

instance (Desug f v, Desug g v) => Desug (f :+: g) v where
    desugAlg :: (f :+: g) v -> v
    desugAlg (Inl a) = desugAlg a
    desugAlg (Inr a) = desugAlg a

instance Desug Val (Term Exp) where
    desugAlg :: Val (Term Exp) -> Term Exp
    desugAlg = inject

instance Desug Op (Term Exp) where
    desugAlg :: Op (Term Exp) -> Term Exp
    desugAlg = inject

instance Desug Sug (Term Exp) where
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


{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

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
-- Datatype composition (Essentially "Either")

data (f :+: g) e
    = Inl (f e)
    | Inr (g e)

instance (Show (f e), Show (g e)) => Show ((f :+: g) e) where
    show (Inl a) = show a
    show (Inr a) = show a

instance (Eq (f e), Eq (g e)) => Eq ((f :+: g) e) where
    (Inl a) == (Inl b) = a == b
    (Inr a) == (Inr b) = a == b
    _ == _ = False

----------------------------------------------------------------------------------------------------

class f :<: g where
    inj :: f a -> g a
    proj :: g a -> Maybe (f a)

instance f :<: f where
    inj = id
    proj = Just

-- instance f :<: (f :+: g) where
--     inj = Inl . inj
--     proj (Inl f) = Just f
--     proj (Inr _) = Nothing

instance (f :<: g) => (f :<: (h :+: g)) where
    inj = Inr . inj
    proj (Inl _) = Nothing
    proj (Inr g) = proj g

{-
-- This one doesn't work: "Duplicate instance declarations"
instance (f :<: h) => (f :<: (h :+: g)) where
    inj = Inl . inj
    proj (Inl h) = proj h
    proj (Inr _) = proj _
-}

inject :: (g :<: f) => g (Term f) -> Term f
inject = Term . inj

project :: (g :<: f) => Term f -> Maybe (g (Term f))
project (Term t) = proj t

----------------------------------------------------------------------------------------------------

data Op e
    = Mult e e
    | Fst e
    deriving (Eq, Show)

iFst :: (Op :<: f) => Term f -> Term f
iFst x = inject (Fst x)
iMult :: (Op :<: f) => Term f -> Term f -> Term f
iMult x y = inject (Mult x y)

data Val e
    = Const Int
    | Pair e e
    deriving (Eq, Show)

iConst :: (Val :<: f) => Int -> Term f
iConst x = inject (Const x)
iPair :: (Val :<: f) => Term f -> Term f -> Term f
iPair x y = inject (Pair x y)

type ExpS = Val :+: Op

type Exp = Term ExpS
type Value = Term Val

const' :: Int -> Exp
const' x    = Term $ Inl $ Const x
pair' :: Exp -> Exp -> Exp
pair' x y   = Term $ Inl $ Pair x y

{- These won't work:
    Our instance (f :<: g) => (f :<: (h :+: g)) implies that the smaller type "f" may
    only occur on the right hand side of ":+:". Thus of course
        (Op :<: ExpS) <= (Op :<: (Val :+: Op)) <= (Op :<: Op)
    But this won't work for Val:
        (Val :<: ExpS) <= (Val :<: (Val :+: Op)) <= (Val :<: Op)
    And we get the following error:
        No instance for (Val :<: Op) arising from a use of `iPair'
-}
--const'      = iConst
--pair'       = iPair     

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

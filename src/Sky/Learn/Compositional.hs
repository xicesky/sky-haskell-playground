
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE DeriveFunctor          #-}

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

unTerm :: Term f -> f (Term f)
unTerm (Term x) = x

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

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl a) = Inl $ fmap f a
    fmap f (Inr a) = Inr $ fmap f a

----------------------------------------------------------------------------------------------------
-- Subsumption, injection and projection

class f :<: g where
    inj :: f a -> g a
    proj :: g a -> Maybe (f a)

instance f :<: f where
    inj = id
    proj = Just

instance {-# OVERLAPPABLE #-} f :<: (f :+: g) where
    inj = Inl . inj
    proj (Inl f) = Just f
    proj (Inr _) = Nothing

instance {-# OVERLAPPABLE #-} (f :<: g) => (f :<: (h :+: g)) where
    inj = Inr . inj
    proj (Inl _) = Nothing
    proj (Inr g) = proj g

inject :: (g :<: f) => g (Term f) -> Term f
inject = Term . inj

project :: (g :<: f) => Term f -> Maybe (g (Term f))
project (Term t) = proj t

----------------------------------------------------------------------------------------------------
-- Catamorphism, Anamorphism

cata :: Functor f => (f a -> a) -> (Term f -> a)
cata f = f . fmap (cata f) . unTerm

ana :: Functor f => (a -> f a) -> (a -> Term f)
ana f = Term . fmap (ana f) . f

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

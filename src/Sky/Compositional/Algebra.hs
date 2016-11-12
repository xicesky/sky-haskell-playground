
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

{- | Compositional data types
See http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.6303&rep=rep1&type=pdf
    http://bahr.io/pubs/files/bahr11wgp-slides%20(full).pdf
And https://hackage.haskell.org/package/compdata
-}

module Sky.Compositional.Algebra where

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

-- | Algebra
type Alg f a = f a -> a

-- | Coalgebra
type CoAlg f a = a -> f a

----------------------------------------------------------------------------------------------------
-- Datatype composition (Essentially "Either")

infixr 6 :+:

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

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
    foldMap :: Monoid m => (a -> m) -> (f :+: g) a -> m
    foldMap f (Inl a) = foldMap f a
    foldMap f (Inr a) = foldMap f a

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
    sequenceA :: Applicative x => (f :+: g) (x a) -> x ((f :+: g) a)
    sequenceA (Inl a) = Inl <$> sequenceA a
    sequenceA (Inr a) = Inr <$> sequenceA a

----------------------------------------------------------------------------------------------------
-- Subsumption, injection and projection

infixl 5 :<:

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


{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Sky.Compositional.Demo where

import Sky.Compositional.Algebra

----------------------------------------------------------------------------------------------------

data Val e
    = Const Int
    | Pair e e
    deriving (Eq, Show, Functor)

instance Foldable Val where
    foldMap :: Monoid m => (a -> m) -> Val a -> m
    foldMap f (Const _) = mempty
    foldMap f (Pair x y) = (mempty `mappend` f x) `mappend` f y

instance Traversable Val where
    sequenceA :: Applicative f => Val (f a) -> f (Val a)
    sequenceA (Const e) = pure (Const e)
    sequenceA (Pair x y) = Pair <$> x <*> y

iConst :: (Val :<: f) => Int -> Term f
iConst x = inject (Const x)
iPair :: (Val :<: f) => Term f -> Term f -> Term f
iPair x y = inject (Pair x y)

-- Unsafe (!!)
projConst :: (Val :<: f) => Term f -> Int
projConst v = case project v of Just (Const n) -> n
projPair :: (Val :<: f) => Term f -> (Term f, Term f)
projPair v = case project v of Just (Pair x y) -> (x, y)

----------------------------------------------------------------------------------------------------

data Op e
    = Mult e e
    | Fst e
    | Snd e
    deriving (Eq, Show, Functor)

instance Foldable Op where
    foldMap :: Monoid m => (a -> m) -> Op a -> m
    foldMap f (Mult x y) = (mempty `mappend` f x) `mappend` f y
    foldMap f (Fst x) = mempty `mappend` f x
    foldMap f (Snd x) = mempty `mappend` f x

instance Traversable Op where
    sequenceA :: Applicative f => Op (f a) -> f (Op a)
    sequenceA (Mult x y) = Mult <$> x <*> y
    sequenceA (Fst x) = Fst <$> x
    sequenceA (Snd x) = Snd <$> x

iFst :: (Op :<: f) => Term f -> Term f
iFst x = inject (Fst x)
iSnd :: (Op :<: f) => Term f -> Term f
iSnd x = inject (Snd x)
iMult :: (Op :<: f) => Term f -> Term f -> Term f
iMult x y = inject (Mult x y)

----------------------------------------------------------------------------------------------------

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
    evalAlg :: f (Term v) -> (Term v)

instance (Eval f v, Eval g v) => Eval (f :+: g) v where
    evalAlg :: (f :+: g) (Term v) -> (Term v)
    evalAlg (Inl a) = evalAlg a
    evalAlg (Inr a) = evalAlg a

instance (Val :<: v) => Eval Val v where
    evalAlg :: Val (Term v) -> Term v
    evalAlg = inject

instance (Val :<: v) => Eval Op v where
    evalAlg :: Op (Term v) -> Term v
    evalAlg (Mult x y) = iConst $ projConst x * projConst y
    evalAlg (Fst p) = fst $ projPair p
    evalAlg (Snd p) = snd $ projPair p

--eval :: ETerm -> VTerm
eval :: (Functor f, Eval f g) => Term f -> Term g
eval = cata evalAlg

class Desug f g where
    desugAlg :: f (Term g) -> Term g

instance (Desug f v, Desug g v) => Desug (f :+: g) v where
    desugAlg :: (f :+: g) (Term v) -> Term v
    desugAlg (Inl a) = desugAlg a
    desugAlg (Inr a) = desugAlg a

instance (Val :<: f, Op :<: f) => Desug Val f where
    desugAlg :: Val (Term f) -> Term f
    desugAlg = inject

instance (Val :<:f, Op :<: f) => Desug Op f where
    desugAlg :: Op (Term f) -> Term f
    desugAlg = inject

instance (Val :<: f, Op :<: f) => Desug Sug f where
    desugAlg :: Sug (Term f) -> Term f
    desugAlg (Neg x) = iConst (-1) `iMult` x
    desugAlg (Swap x) = iPair (iSnd x) (iFst x)

--desug :: ETerm' -> ETerm
desug :: (Functor f, Desug f g) => Term f -> Term g
desug = cata desugAlg

----------------------------------------------------------------------------------------------------
-- Example

expExample :: ETerm
expExample = iFst (iPair (iConst 2) (iConst 3)) `iMult` iConst 5

sugExample :: ETerm'
sugExample = iFst (iSwap (iPair (iConst 3) (iConst 2))) `iMult` iConst 5

main :: IO ()
main = print $ (eval (desug sugExample :: ETerm) :: VTerm)

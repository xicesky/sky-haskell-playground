
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}

module Sky.Isomorphism.Class where

import Data.Tuple (swap)

-- Note: Isomorphisms should be instances of Category!

-- The mightiest of all
class SemiIsomorphism i where
    packSemiIsomorphism :: Monad m => (s -> m a, b -> m t) -> i m s t a b
    unpackSemiIsomorphism :: Monad m => i m s t a b -> (s -> m a, b -> m t)
    -- Derived
    toSemiIsomorphism :: Monad m => (s -> m a) -> (b -> m t) -> i m s t a b
    toSemiIsomorphism sma bmt = packSemiIsomorphism (sma, bmt)
    applySemiIsomorphism :: Monad m => i m s t a b -> s -> m a
    applySemiIsomorphism = fst . unpackSemiIsomorphism
    unapplySemiIsomorphism :: Monad m => i m s t a b -> b -> m t
    unapplySemiIsomorphism = snd . unpackSemiIsomorphism
    revertSemiIsomorphism :: Monad m => i m s t a b -> i m b a t s
    revertSemiIsomorphism = packSemiIsomorphism . swap . unpackSemiIsomorphism

-- class SimpleSemiIsomorphism

data MumuIso m s t a b = MumuIso { _rawMumuIso :: (s -> m a, b -> m t) }

instance SemiIsomorphism MumuIso where
    packSemiIsomorphism = MumuIso
    unpackSemiIsomorphism = _rawMumuIso

-- Operators and stuff
iso :: forall i m s t a b. (SemiIsomorphism i, Monad m) => (s -> m a) -> (b -> m t) -> i m s t a b
iso = toSemiIsomorphism

apply :: forall i m s t a b. (SemiIsomorphism i, Monad m) => i m s t a b -> s -> m a
apply = applySemiIsomorphism

unapply :: forall i m s t a b. (SemiIsomorphism i, Monad m) => i m s t a b -> b -> m t
unapply = unapplySemiIsomorphism

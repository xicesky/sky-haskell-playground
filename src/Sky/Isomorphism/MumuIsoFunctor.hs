
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}          -- IsoFunctorStageOne (MumuIso Maybe x)
{-# LANGUAGE MultiParamTypeClasses #-}      -- IsoFunctorStageTwo
{-# LANGUAGE FunctionalDependencies #-}     -- IsoFunctorStageTwo

{-| Non-polymorphic semi-isomorphisms.
-}

module Sky.Isomorphism.MumuIsoFunctor where

import Prelude hiding (id, (.))
import Control.Category                     -- yay

import Data.Tuple (swap)
import Control.Monad ((<=<))

-- Note: Isomorphisms should be instances of Category.

----------------------------------------------------------------------------------------------------

class SemiIsomorphism i where
    packSemiIsomorphism :: Monad m => (a -> m b, b -> m a) -> i m a b
    unpackSemiIsomorphism :: Monad m => i m a b -> (a -> m b, b -> m a)
    -- Derived
    toSemiIsomorphism :: Monad m => (a -> m b) -> (b -> m a) -> i m a b
    toSemiIsomorphism amb bma = packSemiIsomorphism (amb, bma)
    applySemiIsomorphism :: Monad m => i m a b -> a -> m b
    applySemiIsomorphism = fst . unpackSemiIsomorphism
    unapplySemiIsomorphism :: Monad m => i m a b -> b -> m a
    unapplySemiIsomorphism = snd . unpackSemiIsomorphism
    revertSemiIsomorphism :: Monad m => i m a b -> i m b a
    revertSemiIsomorphism = packSemiIsomorphism . swap . unpackSemiIsomorphism
    convertSemiIsomorphism :: (Monad m, SemiIsomorphism j) => j m a b -> i m a b
    convertSemiIsomorphism = packSemiIsomorphism . unpackSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Operators and stuff

iso :: forall i m a b. (SemiIsomorphism i, Monad m) => (a -> m b) -> (b -> m a) -> i m a b
iso = toSemiIsomorphism

apply :: forall i m a b. (SemiIsomorphism i, Monad m) => i m a b -> a -> m b
apply = applySemiIsomorphism

unapply :: forall i m a b. (SemiIsomorphism i, Monad m) => i m a b -> b -> m a
unapply = unapplySemiIsomorphism

convert :: forall i j m a b. (SemiIsomorphism i, Monad m, SemiIsomorphism j) => i m a b -> j m a b
convert = convertSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Simple semi-isomorphism

newtype MumuIso m a b = MumuIso { _rawMumuIso :: (a -> m b, b -> m a) }

instance Monad m => Category (MumuIso m) where
    id = MumuIso (return, return)
    (MumuIso (applyF, unapplyF)) . (MumuIso (applyG, unapplyG)) =
        MumuIso ((applyF <=< applyG), (unapplyG <=< unapplyF))

instance SemiIsomorphism MumuIso where
    packSemiIsomorphism = MumuIso
    unpackSemiIsomorphism = _rawMumuIso

----------------------------------------------------------------------------------------------------

class IsoFunctorStageOne f where
    isomap1 :: forall a b. MumuIso Maybe a b -> f a -> f b

instance IsoFunctorStageOne (MumuIso Maybe x) where
    isomap1 :: forall a b. MumuIso Maybe a b -> MumuIso Maybe x a -> MumuIso Maybe x b
    isomap1 ab xa = ab . xa

----------------------------------------------------------------------------------------------------

-- But we'd like to make it work with any monad m
class IsoFunctorStageTwo f m | f -> m where
    isomap2 :: forall a b. MumuIso m a b -> f a -> f b

instance (Monad m) => IsoFunctorStageTwo (MumuIso m x) m where
    isomap2 :: forall a b. MumuIso m a b -> MumuIso m x a -> MumuIso m x b
    isomap2 ab xa = ab . xa

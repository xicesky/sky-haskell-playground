
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}       -- Category (MumuIso' m)

{-| Non-polymorphic semi-isomorphisms.
-}

module Sky.Isomorphism.Simple where

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

----------------------------------------------------------------------------------------------------

newtype MumuIso m a b = MumuIso { _rawMumuIso :: (a -> m b, b -> m a) }

instance Monad m => Category (MumuIso m) where
    id = MumuIso (return, return)
    (MumuIso (applyF, unapplyF)) . (MumuIso (applyG, unapplyG)) =
        MumuIso ((applyF <=< applyG), (unapplyG <=< unapplyF))

instance SemiIsomorphism MumuIso where
    packSemiIsomorphism = MumuIso
    unpackSemiIsomorphism = _rawMumuIso

----------------------------------------------------------------------------------------------------

-- Operators and stuff
iso :: forall i m a b. (SemiIsomorphism i, Monad m) => (a -> m b) -> (b -> m a) -> i m a b
iso = toSemiIsomorphism

apply :: forall i m a b. (SemiIsomorphism i, Monad m) => i m a b -> a -> m b
apply = applySemiIsomorphism

unapply :: forall i m a b. (SemiIsomorphism i, Monad m) => i m a b -> b -> m a
unapply = unapplySemiIsomorphism

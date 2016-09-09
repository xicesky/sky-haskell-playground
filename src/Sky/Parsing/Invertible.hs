
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}                 -- Required to use SemiIso apparently

module Sky.Parsing.Invertible where

import Data.Functor.Identity
import Sky.Isomorphism.Class
import Sky.Isomorphism.SemiIso

infixl 4 <$>

class IsoFunctor f where
    (<$>) :: forall i a b. (SemiIsomorphism i) => i Maybe a a b b -> f a -> f b

infixl 4 <*>

class IsoApplicative f where
    (<*>) :: forall i a b. (SemiIsomorphism i) => f (i Maybe a a b b) -> f a -> f b

-- Doesn't work with raw SemiIso: f (SemiIso' ...) requires ImpredicativeTypes


{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TypeSynonymInstances #-}

module Sky.Implementations.Isomorphism.MonoIso where

import Sky.Classes.Isomorphism
import Sky.Classes.Isomorphism.Monomorphic
import Sky.Classes.IsoFunctor
import Sky.Implementations.Isomorphism

import Data.Functor.Identity
import Control.Category (Category)
import qualified Control.Category as Cat

----------------------------------------------------------------------------------------------------
-- Commonly used isomorphisms

type Iso a b = PackedMonoSemiIso Identity a b

isoCons :: Iso (a, [a]) [a]
isoCons = iso (\(x,xs) -> x:xs) (\(x:xs) -> (x,xs))

isoAlternative :: forall a. (a -> Bool) -> Iso (Either a a) a
isoAlternative decision = iso joinAlt splitAlt where
    joinAlt :: Either a a -> a
    joinAlt (Left a) = a
    joinAlt (Right a) = a
    splitAlt x = if decision x then Left x else Right x

isoAlt :: forall a. (a -> Bool) -> Iso (Either a a) a
isoAlt = isoAlternative

isoFixedRight :: b -> Iso (a,b) a
isoFixedRight fixed = iso (\(a,b) -> a) (\a -> (a, fixed))

isoFixedLeft :: a -> Iso (a,b) b
isoFixedLeft fixed = iso (\(a,b) -> b) (\b -> (fixed, b))

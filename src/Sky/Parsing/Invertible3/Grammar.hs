
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE BangPatterns           #-}

module Sky.Parsing.Invertible3.Grammar where

import Prelude hiding (id, (.))
import Control.Category (Category(..))

import Sky.Parsing.Invertible3.Isomorphism
import Data.Data (Data)

data Example
    = Example1
    | Example2 Int
    | Example3 Int Int
    --  ...
    deriving (Data)

example1 :: Iso Example ()
example1 = partialIso (Example1 {}) forward backward where
    forward (Example1)  = ()
    forward _           = error $ "Partial iso"
    backward ()         = Example1

example2 :: Iso Example Int
example2 = partialIso (Example2 {}) forward backward where
    forward (Example2 i) = i
    forward _           = error $ "Partial iso"
    backward i          = Example2 i

example3 :: Iso Example (Int, Int)
example3 = partialIso (Example3 {}) forward backward where
    forward (Example3 i j) = (i, j)
    forward _           = error $ "Partial iso"
    backward (i, j)     = Example3 i j

eval :: a -> String
eval !a = "Ok."

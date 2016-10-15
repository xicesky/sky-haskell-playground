
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE TemplateHaskell        #-}

module Sky.Parsing.Invertible3.Grammar where

import Prelude hiding (id, (.))
import Control.Category (Category(..))

import Sky.Parsing.Invertible3.Isomorphism
import Sky.Parsing.Invertible3.TH
import Data.Data (Data)

-- Temp for GHCI
import Language.Haskell.TH

data Example
    = Example1
    | Example2 Int
    | Example3 Int Int
    -- | Example4 { a :: Int, b :: Bool, c :: String }
    --  ...
    deriving (Show, Data)

makeIsomorphism "example1'" [|Example1|]
makeIsomorphisms ''Example defaultIsoNamer

exampleM1 :: Iso () Example
exampleM1 = $(isoConstr 'Example1)

exampleM2 :: Iso Int Example
exampleM2 = $(isoConstr 'Example2)

exampleM3 :: Iso (Int, Int) Example
exampleM3 = $(isoConstr 'Example3)

{-
example1Custom :: Iso Example ()
example1Custom = partialIso (Example1 {}) forward backward where
    forward (Example1)  = ()
    forward _           = error $ "Partial iso"
    backward ()         = Example1

example2Custom :: Iso Example Int
example2Custom = partialIso (Example2 {}) forward backward where
    forward (Example2 i) = i
    forward _           = error $ "Partial iso"
    backward i          = Example2 i

example3Custom :: Iso Example (Int, Int)
example3Custom = partialIso (Example3 {}) forward backward where
    forward (Example3 i j) = (i, j)
    forward _           = error $ "Partial iso"
    backward (i, j)     = Example3 i j
-}

eval :: a -> String
eval !a = "Ok."

-- Run e.g.:
-- ghci -ddump-splices -XDeriveDataTypeable -XTemplateHaskell -isrc src/Sky/Parsing/Invertible3/Grammar.hs

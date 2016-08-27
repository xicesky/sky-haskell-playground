
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Sky.ControlLens.TestControlLens where

import Data.HashMap.Strict
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT, runStateT, execStateT)
import Data.Functor.Identity

import Control.Lens

-- Basic code generator monad --------------------------------------------------

data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo


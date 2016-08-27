
module Sky.ControlLens.ExternalDefs where

-- This is defined externally and we can't modify it
-- How can we make lenses for it?

data Type = TInt | TDouble | TVoid
  deriving (Eq, Show)

data Definition = Definition String Type
  deriving (Eq, Show)

data Module = Module {
    moduleName :: String
  , definitions ::  [Definition]
  }
  deriving (Eq, Show)

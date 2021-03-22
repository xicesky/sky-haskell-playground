
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE RankNTypes             #-}

{-# LANGUAGE TemplateHaskell        #-}     -- derive

{- How to GHCI this:
    stack exec ghci -- -isrc -XFlexibleContexts -XTypeFamilies -XTemplateHaskell -ddump-splices src/Sky/Compositional/Lambda.hs
-}

module Sky.Compositional.Lambda where

import Data.Comp.Param
--import Data.Comp.Param.Algebra
import Data.Comp.Param.Derive               -- derive
import Control.Monad.Reader

----------------------------------------------------------------------------------------------------

data CoreExpr v e
    = Lambda (v -> e)
    | App e e

$(derive [smartConstructors, makeDifunctor, makeShowD, makeEqD, makeOrdD]
         [''CoreExpr])

----------------------------------------------------------------------------------------------------

class Pretty f where
  prettyAlg :: Alg f ([String] -> String)

$(derive [liftSum] [''Pretty])

pretty :: (Difunctor f, Pretty f) => Term f -> String
pretty t = cata prettyAlg t (nominals 1)
    where nominals n = ('x' : show n) : (nominals (n + 1))

instance Pretty CoreExpr where
    prettyAlg (Lambda f) (name : names)
        -- f :: ([String] -> String) -> [String] -> String
        = "(\\" ++ name ++ ". " ++ f (const name) names ++ ")"
    prettyAlg (App fe e) (names)
        -- fe, e :: [String] -> String
        = fe names ++ " " ++ e names

----------------------------------------------------------------------------------------------------

type NameSupply = [String]
defaultNameSupply :: NameSupply
defaultNameSupply = ['x' : show n | n <- [0..]]

type SupplM = Reader NameSupply

newName :: SupplM String
newName = do
    (name : rest) <- ask
    return name

subEnv :: SupplM String -> SupplM String
subEnv = local tail

class PrettyM f where
    --prettyMAlg :: AlgM SupplM f String        -- AlgM is wrong, we need to modify the monad
    prettyMAlg :: Alg f (SupplM String)
    -- prettyMAlg :: f (SupplM String) (SupplM String) -> SupplM String

$(derive [liftSum] [''PrettyM])

instance PrettyM CoreExpr where
    prettyMAlg :: CoreExpr (SupplM String) (SupplM String) -> SupplM String 
    prettyMAlg (Lambda f) = do  -- f :: SupplM String -> SupplM String
        name <- newName
        subEnv $ do
            inner <- f (return name)
            return $ "(\\" ++ name ++ ". " ++ inner ++ ")"
    prettyMAlg (App fe e) = do
        left <- fe
        right <- e
        return $ left ++ " " ++ right

prettyM :: (Difunctor f, PrettyM f) => Term f -> String
prettyM t = let
    expr :: SupplM String
    expr = cata prettyMAlg t
    in runReader expr defaultNameSupply

----------------------------------------------------------------------------------------------------

class OccurCount f where
    occurCountAlg :: Alg f (Int -> Int)

$(derive [liftSum] [''OccurCount])

instance OccurCount CoreExpr where
    occurCountAlg :: CoreExpr (Int -> Int) (Int -> Int) -> Int -> Int
    occurCountAlg (Lambda f) vCounts    -- f :: (Int -> Int) -> (Int -> Int)
        = f (const vCounts) 0
    occurCountAlg (App fe e) vCounts
        = fe 0 + e 0

occurCount' :: (Difunctor f, OccurCount f) => Term f -> Int
occurCount' t = cata occurCountAlg t 1

--occurCount1 :: (Difunctor f, OccurCount f) => (forall a. Trm f a) -> Int
occurCount1 :: (Difunctor f, OccurCount f) => (forall a. Cxt NoHole f a ()) -> Int
--occurCount1 :: (forall a. Cxt NoHole CoreExpr a ()) -> Int
occurCount1 t = occurCount' (Term t)

occurCount2 :: (Difunctor f, OccurCount f, CoreExpr :<: f) =>
                (forall a. Cxt h0 f0 a b0 -> Cxt NoHole f a ()) -> Int
--occurCount2 :: (forall a. Cxt h0 f0 a b0 -> Cxt NoHole CoreExpr a ()) -> Int
occurCount2 t = occurCount' (Term $ iLambda t)

--Lambda :: (v -> e) -> CoreExpr v e
--

----------------------------------------------------------------------------------------------------

exampleYX :: Term CoreExpr
exampleYX = Term $ t where
    t :: Trm CoreExpr a
    t = iLambda $ \x ->
        iLambda $ \y ->
        iApp y x

exampleXX :: Term CoreExpr
exampleXX = Term $ t where
    t :: Trm CoreExpr a
    t = iLambda $ \x ->
        iLambda $ \y ->
        iApp x x

----------------------------------------------------------------------------------------------------

-- Demo for occurCount'
demo_occurCount' :: [Int]
demo_occurCount' =
    [ occurCount' exampleYX     -- "x" appears 1 time
    , occurCount' exampleXX     -- "x" appears 2 times
    ]

-- Demo for occurCount2
demo_occurCount2 :: Int
demo_occurCount2 = occurCount2 $ t where
    t :: Cxt NoHole CoreExpr a () -> Cxt NoHole CoreExpr a ()
    t = \x -> x -- iApp x x

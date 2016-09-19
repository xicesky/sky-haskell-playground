
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DeriveGeneric #-}

module Sky.Parsing.Invertible2 where

import Prelude hiding (lookup, (!!), print, (.), id)
import Data.Functor.Identity
import Control.Category
import Control.Monad (join)
import Data.List (sort, intercalate)

import Debug.Trace
import Data.Function ((&))
import GHC.Generics (Generic)

import Sky.Classes.Isomorphism.Monomorphic
import Sky.Implementations.Isomorphism
import Sky.Implementations.Isomorphism.MonoIso
import Sky.Util.NewContainer
import Sky.Util.NewGraphLike

class (Show a, Eq a, Ord a, Hashable a) => D a
instance D Char
instance D a => D [a]

data TokenSet token
    = AnyToken
    | SomeTokens (HashSet token)
    | SingleToken token
    deriving (Show, Eq, Generic)
instance Hashable token => Hashable (TokenSet token)
--instance (Show token, Eq token, Hashable token) => D (TokenSet token)

-- More like regex, sets and sequences of tokens...
data Syntax' token ast a where
    Empty       :: Syntax' token ast a                                                      -- Fail
    Pure        :: a -> Syntax' token ast a                                                 -- Generate a without input
    Token       :: Iso token a -> TokenSet token -> Syntax' token ast a               -- Accept a certain set of tokens
    Sequence    :: (D x, D y) => Iso (x,y) a -> Syntax' token ast x -> Syntax' token ast y -> Syntax' token ast a -- Sequence
    Alternative :: (D x, D y) => Iso (Either x y) a -> Syntax' token ast x -> Syntax' token ast y -> Syntax' token ast a   -- Alternative
    Reference   :: String -> Syntax' token ast ast                                          -- Call to a reference

type Syntax token ast = HashMap String (Syntax' token ast ast)

data DemoAST
    = Literal Int
    | Add DemoAST DemoAST
    -- | Variable String
    deriving (Show, Eq, Ord, Generic)
instance Hashable DemoAST
instance D DemoAST

tokenSet :: (Ord a, Hashable a) => [a] -> TokenSet a
tokenSet = SomeTokens . fromList

applyIso :: Iso a b -> Syntax' token ast a -> Syntax' token ast b
applyIso f (Empty) = Empty
applyIso f (Pure a) = Pure $ apply f a
applyIso f (Token g recg) = Token (f . g) recg
applyIso f (Sequence g a b) = Sequence (f . g) a b
applyIso f (Alternative g a b) = Alternative (f . g) a b
applyIso f (Reference name) = error $ "Cannot apply ismorphism to reference: " ++ name

many :: D a => Syntax' tok ast a -> Syntax' tok ast [a]
many single = Alternative (isoAlt null) (Pure []) $ Sequence isoCons single (many single)

digit :: Syntax' Char ast Char
digit = Token id $ tokenSet "0123456789"

number :: Syntax' Char a DemoAST
number = applyIso (iso toLiteral fromLiteral) (many digit) where
    toLiteral :: String -> DemoAST
    toLiteral = Literal . read
    fromLiteral :: DemoAST -> String
    fromLiteral (Literal i) = show i

--number :: Syntax' String a Int
--number = 

numberAST :: Syntax' String DemoAST DemoAST
numberAST = applyIso (iso Literal (\(Literal i) -> i)) (Token (iso read show) (tokenSet $ map return "0123456789"))

lit :: (Ord a, Hashable a) => a -> Syntax' a ast a
lit a = Token (iso id id) (SingleToken a)

parens :: forall ast a. (D a) => Syntax' String ast a -> Syntax' String ast a
parens x = Sequence (isoFixedLeft "(") (lit "(") (Sequence (isoFixedRight ")") x (lit ")"))

isoAdd :: Iso (DemoAST, DemoAST) DemoAST
isoAdd = iso (\(a,b) -> Add a b) (\x -> traceShow x $ x & \(Add a b) -> (a, b))

addExpr :: Syntax' String DemoAST DemoAST
addExpr = Sequence isoAdd (Reference "Simple") (Sequence (isoFixedLeft "+") (lit "+") (Reference "Simple"))

isLiteral :: DemoAST -> Bool
isLiteral (Literal _) = True
isLiteral _           = False

isAdd :: DemoAST -> Bool
isAdd (Add _ _) = True
isAdd _         = False

demoSyntax :: Syntax String DemoAST
demoSyntax = fromList
    [ ("Literal"    , numberAST)
    , ("Simple"     , Alternative (isoAlt isLiteral) (Reference "Literal") (parens (Reference "Expr")))
    , ("Expr"       , Alternative (isoAlt isAdd) (addExpr) (Reference "Simple"))
    ]

demoAST = (Literal 2) `Add` (Literal 3 `Add` Literal 4)
--demoString = ["2", "+", "3"]

print :: (D ast, D a) => Syntax token ast -> Syntax' token ast a -> a -> Maybe [token]
print syn   (Empty)                 _   | trace "Empty" True = Nothing
print syn   (Token iso _)           ast | trace "Token" True = Just $ [unapply iso ast]
print syn   (Pure ast2)             ast | trace "Pure" True = if (ast == ast2) then Just [] else Nothing
print syn   (Sequence iso sa sb)    ast | trace "Sequence" True = do
        (a, b) <- return $ unapply iso ast
        pa <- print syn sa a
        pb <- print syn sb b
        return (pa ++ pb)
print syn   (Alternative iso sa sb) ast | trace "Alternative" True = case unapply iso ast of
        Left x -> print syn sa x
        Right y -> print syn sb y
--print syn   (Name _ a)                ast = print a ast
print syn   (Reference name)        ast | trace ("Reference " ++ name ++ ": " ++ show ast) True = print syn (syn !! name) ast

grammar1 :: (D token) => Syntax' token ast a -> String
grammar1 (Empty)             = "(Fail)"
grammar1 (Token _ toks)       = case toks of
    AnyToken        -> "."
    SomeTokens set  -> intercalate "|" $ fmap show $ sort $ toList set
    SingleToken tok -> show tok
grammar1 (Pure _)            = "(Pure)"
grammar1 (Sequence _ a b)    = grammar1 a ++ " " ++ grammar1 b
grammar1 (Alternative _ a b) = grammar1 a ++ " | " ++ grammar1 b
grammar1 (Reference name)    = name

grammar :: forall token ast. (D token, D ast) => Syntax token ast -> String
grammar syn = join $ map rule $ toList syn where
    rule :: (String, Syntax' token ast a) -> String
    rule (name, g) = name ++ " = " ++ grammar1 g ++ "\n"

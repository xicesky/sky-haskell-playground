
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}           -- Required for handling SemiIsos (Non type-variable argument in the constraint)

module Sky.Lens.SemiIsoDemo where

import Data.Functor.Identity
import Data.Char (toUpper, toLower)
import Control.Monad.Except

import Sky.Isomorphism.SemiIso

-- Let's do a simple semi-isomorphism:
-- Transform strings between "all lowercase" and "all uppercase"
upperIso :: SemiIso' Identity String String
upperIso = semiIso toUpperStr toLowerStr where
    toUpperStr :: String -> Identity String
    toUpperStr = return . (fmap toUpper)
    toLowerStr :: String -> Identity String
    toLowerStr = return . (fmap toLower)

-- Now let's do one that can fail:
-- This isomorphism fails on "_"
upperIsoFail :: SemiIso' Maybe String String
upperIsoFail = semiIso toUpperStr toLowerStr where
    checkUnderscore :: (Char -> Char) -> Char -> Maybe Char
    checkUnderscore f '_' = Nothing
    checkUnderscore f x = Just (f x)

    toUpperStr :: String -> Maybe String
    toUpperStr = traverse (checkUnderscore toUpper)

    toLowerStr :: String -> Maybe String
    toLowerStr = traverse (checkUnderscore toLower)

-- Maybe you'd like an error message instead?
upperIsoErr :: SemiIso' (Either String) String String
upperIsoErr = semiIso toUpperStr toLowerStr where
    checkUnderscore :: (Char -> Char) -> Char -> Either String Char
    checkUnderscore f '_'   = Left "Encountered underscore"
    checkUnderscore f x     = Right (f x)

    toUpperStr :: String -> (Either String) String
    toUpperStr = traverse (checkUnderscore toUpper)

    toLowerStr :: String -> (Either String) String
    toLowerStr = traverse (checkUnderscore toLower)

-- We can build a nice one between "Maybe a" and "[a]" to have something
-- that actually changes types. Also we will be using the "Except" monad
-- instead of "Either" because it handles better
list1iso :: forall a. (Show a) => SemiIso' (Except String) (Maybe a) [a]
list1iso = semiIso maybeToList listToMaybe where
    maybeToList :: Maybe a -> Except String [a]
    maybeToList (Nothing)   = return []
    maybeToList (Just x)    = return [x]
    listToMaybe :: [a] -> Except String (Maybe a)
    listToMaybe []          = return Nothing
    listToMaybe [x]         = return (Just x)
    listToMaybe xs          = throwError $ "List of multiple elements: " ++ show xs

demo :: IO ()
demo = do
    putStrLn $ "applySemiIso upperIso \"abcDEF\"                      = " ++ show (applySemiIso upperIso "abcDEF")
    putStrLn $ "unapplySemiIso upperIso \"abcDEF\"                    = " ++ show (unapplySemiIso upperIso "abcDEF")
    putStrLn $ "applySemiIso (reverseSemiIso upperIso) \"abcDEF\"     = " ++ show (applySemiIso (reverseSemiIso upperIso) "abcDEF")
    putStrLn ""
    putStrLn $ "applySemiIso upperIsoFail \"abcDEF\"                  = " ++ show (applySemiIso upperIsoFail "abcDEF")
    putStrLn $ "applySemiIso upperIsoFail \"abc_DEF\"                 = " ++ show (applySemiIso upperIsoFail "abc_DEF")
    putStrLn $ "unapplySemiIso upperIsoFail \"abc_DEF\"               = " ++ show (unapplySemiIso upperIsoFail "abc_DEF")
    putStrLn ""
    putStrLn $ "applySemiIso upperIsoErr \"abcDEF\"                   = " ++ show (applySemiIso upperIsoErr "abcDEF")
    putStrLn $ "applySemiIso upperIsoErr \"abc_DEF\"                  = " ++ show (applySemiIso upperIsoErr "abc_DEF")
    putStrLn $ "unapplySemiIso upperIsoErr \"abc_DEF\"                = " ++ show (unapplySemiIso upperIsoErr "abc_DEF")
    putStrLn ""
    putStrLn $ "applySemiIso list1iso (Nothing)                     = " ++ show (runExcept $ applySemiIso list1iso (Nothing :: Maybe Char))
    putStrLn $ "applySemiIso list1iso (Just 'x')                    = " ++ show (runExcept $ applySemiIso list1iso (Just 'x'))
    putStrLn $ "unapplySemiIso list1iso ([])                        = " ++ show (runExcept $ unapplySemiIso list1iso ([] :: [Char]))
    putStrLn $ "unapplySemiIso list1iso (['y'])                     = " ++ show (runExcept $ unapplySemiIso list1iso (['y']))
    putStrLn $ "unapplySemiIso list1iso (['a','b','c'])             = " ++ show (runExcept $ unapplySemiIso list1iso (['a','b','c']))

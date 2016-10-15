
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE TemplateHaskell        #-}

{-  Automatically derive isomorphisms from algebraic type definitions.
-}
module Sky.Parsing.Invertible3.TH where

import Data.Char (toLower)
import Data.List (find)

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Control.Monad
import Sky.Parsing.Invertible3.Isomorphism

----------------------------------------------------------------------------------------------------
-- General utilities

tuplE :: [Name] -> Q Exp
tuplE []    = [|()|]
tuplE [v]   = varE v
tuplE xs    = tupE (map varE xs)

tuplP :: [Name] -> Q Pat
tuplP []    = [|()|] >>= \(ConE n) -> conP n []
tuplP [v]   = varP v
tuplP xs    = tupP (map varP xs)

apps :: Q Exp -> [Q Exp] -> Q Exp
apps f [] = f
apps f (x:xs) = apps (f `appE` x) xs

fName :: Dec -> Q Name
fName (FunD n _) = return n
fName fdec = error $ "Not a function declaration: " ++ show fdec

fRef :: Dec -> Q Exp
fRef = fmap VarE . fName

----------------------------------------------------------------------------------------------------
-- Constructor infos
{-
    Different types of constructors need different handling (NormalC, RecC, InfixC, ...).
    These functions abstract those differences using a common info structor "ConstrInfo".
-}

data ConstrInfo = ConstrInfo Name [Type] Con
    deriving (Eq, Ord, Show)

-- Create a ConstrInfo from a constructor
conInfo :: Con -> ConstrInfo
conInfo con@(NormalC name bangtypes)     = ConstrInfo name (map snd bangtypes) con
conInfo con@(RecC name varbangtypes)     = ConstrInfo name (map getT varbangtypes) con where getT (_,_,t) = t
conInfo con@(InfixC bt1 name bt2)        = ConstrInfo name [snd bt1, snd bt2] con

-- Get all constructors of a type
getConstrs :: Name -> Q [ConstrInfo]
getConstrs name = do
    info <- reify name
    case info of
        TyConI (DataD _ name' tyVarBndr constrs _) -> return $ map conInfo constrs
        _ -> error $ "Cannot handle: " ++ show info

-- Get the ConstrInfo from a constructor name
reifyConstr :: Name -> Q ConstrInfo
reifyConstr cName = do
    info <- reify cName
    case info of
        DataConI _ _ parentName _ -> do
            constrs <- getConstrs parentName
            case find (\(ConstrInfo n _ _) -> n == cName) constrs of
                Just con -> return con
                Nothing -> error $ "Could not find constructor named: " ++ show cName
        _ -> error $ "Not a data constructor: " ++ show cName

-- Get the number of arguments a constructor expects
conArity :: ConstrInfo -> Int
conArity (ConstrInfo _ types _) = length types

-- Generate empty constructor application (i.e: Constr {})
emptyConstr :: ConstrInfo -> Q Exp
emptyConstr (ConstrInfo cName _ _) = recConE cName []

-- Apply a constructor to a number of variables
applyConstr :: ConstrInfo -> [Name] -> Q Exp
applyConstr con@(ConstrInfo cName _ _) vars =
    if length vars /= conArity con
        then error $ "Constructor " ++ show cName ++ " applied to wrong number of arguments: " ++ show vars
        else apps (conE cName) (map varE vars)

-- Match a constructor to a number of variables
matchConstr :: ConstrInfo -> [Name] -> Q Pat
matchConstr con@(ConstrInfo cName _ _) vars =
    if length vars /= conArity con
        then error $ "Constructor " ++ show cName ++ " applied to wrong number of arguments: " ++ show vars
        else conP cName (map varP vars)

----------------------------------------------------------------------------------------------------

type IsoNamer = String -> String

defaultIsoNamer :: IsoNamer
defaultIsoNamer = fmap toLower

-- Generate a name from a constructor
nameFromConstr :: IsoNamer -> ConstrInfo -> Q Name
nameFromConstr namer (ConstrInfo cName _ _) = do
    return $ mkName $ namer $ nameBase cName

----------------------------------------------------------------------------------------------------

-- Generate a "forward" function for a constructor
-- E.g. for "Example3 Int Int" generate "forward :: (Int, Int) -> Example"
mk_forward :: ConstrInfo -> Q Dec
mk_forward constr = do
    nmForward <- newName "forward"
    vars <- replicateM (conArity constr) (newName "x")
    funD nmForward
        [   clause [tuplP vars] (normalB $ applyConstr constr vars) []      -- forward (x1, x2) = Constr x1 x2
        ]

mk_backward :: ConstrInfo -> Q Dec
mk_backward constr = do
    nmBackward <- newName "backward"
    vars <- replicateM (conArity constr) (newName "y")
    funD nmBackward
        [   clause [matchConstr constr vars] (normalB $ tuplE vars) []      -- backward (Constr y1 y2) = (y1, y2)
        ,   clause [wildP] (normalB $ [| error "Partial iso" |]) []         -- backward _ = error $ ...
        ]

mk_iso :: ConstrInfo -> Q Exp
mk_iso constr = do
    forward <- mk_forward constr
    backward <- mk_backward constr
    letE [return forward, return backward]
        $ [|partialIsoR|] `appE` (emptyConstr constr) `appE` (fRef forward) `appE` (fRef backward)

makeIsomorphisms :: Name -> IsoNamer -> Q [Dec]
makeIsomorphisms name isoNamer = do
    info <- reify name
    constrs <- getConstrs name
    runIO $ do
        putStrLn "----------------------------------------------------------------------------------------------------"
        putStrLn "-- Template Haskell:"
        putStrLn $ show name
        putStrLn $ show info
        putStrLn ""
        forM_ constrs $ \constr -> putStrLn ("    " ++ show constr)
    forM constrs $ \constr -> do
        isoName <- nameFromConstr isoNamer constr
        valD (varP isoName) (normalB $ mk_iso constr) []
    --return []

makeIsomorphism :: String -> Q Exp -> Q [Dec]
makeIsomorphism name conE = do
    ConE cName <- conE
    constr <- reifyConstr cName
    isoName <- return $ mkName name
    fmap return $ valD (varP isoName) (normalB $ mk_iso constr) []

isoConstr :: Name -> Q Exp
isoConstr cName = do
    constr <- reifyConstr cName
    mk_iso constr

{-
:{
runQ [|
let
    forward = undefined
    backward = undefined
    in partialIso undefined forward backward
|]
:}
-}

{-
$(reify 'Example1 >>= stringE . show)
-}
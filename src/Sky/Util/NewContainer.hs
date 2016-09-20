
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Sky.Util.NewContainer
    (    module Data.Hashable
    ,   Constructible(..)
    ,   Collapseable(..)
    ,   Intersectable(..)
    ,   HasLookup(..)
    ,   MapLike(..)
    ,   MapFoldable(..)
    ,   TreeSet
    ,   HashSet
    ,   TreeMap
    ,   HashMap
    ) where
-- The old one used functional dependencies, the new one uses type families

----------------------------------------------------------------------------------------------------
{-
    This module tries to present an consistent interface for containers, specifically for
    Data.Set, Data.Map and Hash-* equivalents. Some functions from the prelude are redefined,
    so best import it like this:
        import Prelude hiding (lookup, (!!))
        import Util.Container
-}
----------------------------------------------------------------------------------------------------

{- TODO:
    - This resembles the stuff from:
        https://hackage.haskell.org/package/collections-api-1.0.0.0/docs/Data-Collections.html
    - This module needs a lot of cleanup, since most of the stuff is fixed in base in the meantime:
        - Every data structure should be a Monoid, so we get mempty, ...
            - Singleton + mappend = fromList?
        - We have Foldable which includes toList, length, null, elem
        - Traverseable
        - IsList contains the item type (k, v) and toList, fromList
        - What class only contains "pure" but not "fapply" (aka <*>) ?

    - Definitely not in base are
        - mapWithKey, foldrWithKey, traverseWithKey ...

-}

import           Prelude             (Bool (..), Eq (..), Int, Ord (..), String,
                                      error, flip, fst, head, length, snd, tail,
                                      undefined, ($), (++), (.), (==))
import qualified Prelude

import           Data.Foldable       hiding (toList)
import           Data.Maybe
import           Data.Monoid

import           Data.Map            (Map)
import qualified Data.Map            as DataMap
import           Data.Set            (Set)
import qualified Data.Set            as DataSet

import           Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet

type TreeSet v = DataSet.Set v
type HashSet v = HashSet.HashSet v
type TreeMap k v = DataMap.Map k v
type HashMap k v = HashMap.HashMap k v

----------------------------------------------------------------------------------------------------

class BaseContainer c where
    type ElemT c :: *

class (BaseContainer c) => Constructible c where
    empty :: c
    insert :: ElemT c -> c -> c
    -- Optional
    singleton :: ElemT c -> c
    singleton v = insert v empty
    fromList :: [ElemT c] -> c
    fromList [] = empty
    fromList (x:xs) = insert x (fromList xs)

class (BaseContainer c) => Collapseable c where
    c_foldMap :: Monoid m => (ElemT c -> m) -> c -> m
    -- Optional
    elements :: c -> [ElemT c]
    elements c = c_foldMap (\x -> [x]) c
    size :: c -> Int
    size c = length (elements c)
    isEmpty :: c -> Bool
    isEmpty c = Prelude.null (elements c)
    toList :: c -> [ElemT c]
    toList = elements

class (BaseContainer c) => Intersectable c where
    {- Note: These functions may be left-biased, e.g. for maps, they do not
        neccessarily commute with "contains", but certainly with "hasKey"
    -}
    union :: c -> c -> c
    --union a b = fromList $ (toList a) ++ (toList b)
    intersection :: c -> c -> c
    --intersection a b = fromList $ Prelude.filter (b `contains`) (toList a)

class (Constructible c, Collapseable c, Intersectable c) => Container c where
    contains :: c -> ElemT c -> Bool
    --contains c v = not $ isEmpty $ intersection c $ singleton v

infixl 9 !!

class HasLookup c where
    type KeyT c :: *
    type ValueT c :: *
    lookup :: KeyT c -> c -> Maybe (ValueT c)
    isMemberOf :: KeyT c -> c -> Bool
    isMemberOf k m = isJust $ lookup k m
    (!!) :: c -> KeyT c -> ValueT c
    (!!) m k = fromMaybe (error $ "Invalid key for lookup (!!)") $ lookup k m

class (HasLookup m, Collapseable m, (KeyT m, ValueT m) ~ ElemT m) => MapLike m where
    mapInsert :: KeyT m -> ValueT m -> m -> m
    mapDelete :: KeyT m -> m -> m
    -- Additionally
    keys :: m -> [KeyT m]
    keys m = Prelude.map fst (elements m)
    keysSet :: (Container c, ElemT c ~ KeyT m) => m -> c
    keysSet = fromList . keys
    values :: m -> [ValueT m]
    values m = Prelude.map snd (elements m)
    --mapInsert k v m = insert (k,v) m
    hasKey :: m -> (KeyT m) -> Bool
    hasKey m k = isMemberOf k m

class MapFoldable m where
    mapWithKey :: (k -> va -> vb) -> m k va -> m k vb
    --mapWithKey f m = fromList $ map (\(k,v) -> f
    foldrWithKey :: (k -> v -> a -> a) -> a -> m k v -> a
    foldlWithKey :: (a -> k -> v -> a) -> a -> m k v -> a

----------------------------------------------------------------------------------------------------

instance BaseContainer [v] where
    type ElemT [v] = v

instance Constructible [v] where
    empty = []
    insert v l = v:l
    singleton v = [v]
    fromList = Prelude.id

instance Collapseable [v] where
    c_foldMap = foldMap
    elements = Prelude.id
    size l = length l
    isEmpty l = Prelude.null l
    toList = Prelude.id

instance (Eq v) => Intersectable [v] where
    union a b = a ++ b -- nub!?
    intersection a b = Prelude.filter (b `contains`) a

instance (Eq v) => Container [v] where
    contains l v = Prelude.elem v l

instance HasLookup [v] where
    type KeyT [v] = Int
    type ValueT [v] = v
    lookup i l = if (i < 0) Prelude.|| (i >= size l)
        then Nothing
        else Just $ (Prelude.!!) l i

{-
type List v = [] v
type Tuple k v = (,) k v
--type ListAsMap k v = [(k,v)]
type ListAsMap k v = List (Tuple k v)   -- eliminate k and v how!?!?

instance (Eq k, Eq v) => MapLike (ListAsMap k v) k v where
    lookup k l = Prelude.lookup k l

instance MapFoldable ListAsMap where
    mapWithKey f l = Prelude.map f' l where
        f' (k,v) = (k, f k v)
    foldWithKey f a l = Prelude.foldr f' a l where
        f' a (k,v) = f k v a
-}

{-
-- It works with newtype at least
newtype ListAsMap k v = ListAsMap [(k,v)]
lm_toList (ListAsMap x) = x

instance MapFoldable ListAsMap where
    mapWithKey f l = ListAsMap $ Prelude.map f' (lm_toList l) where
        f' (k,v) = (k, f k v)
    --foldWithKey :: (k -> v -> a -> a) -> a -> m k v -> a
    foldWithKey f a l = Prelude.foldr f' a (lm_toList l) where
        f' (k,v) a = f k v a
-}

----------------------------------------------------------------------------------------------------

instance BaseContainer (TreeSet v) where
    type ElemT (TreeSet v) = v

instance (Ord v) => Constructible (TreeSet v) where
    empty = DataSet.empty
    insert v m = DataSet.insert v m
    singleton v = DataSet.singleton v
    fromList = DataSet.fromList

instance Collapseable (TreeSet v) where
    c_foldMap = foldMap
    elements = DataSet.toList
    size = DataSet.size
    isEmpty = DataSet.null
    toList = DataSet.toList

instance (Ord v) => Intersectable (TreeSet v) where
    union a b = DataSet.union a b
    intersection a b = DataSet.intersection a b

instance (Hashable v, Ord v) => Container (TreeSet v) where
    contains m v = DataSet.member v m

----------------------------------------------------------------------------------------------------

-- HashSet is missing Foldable

instance BaseContainer (HashSet v) where
    type ElemT (HashSet v) = v

instance (Hashable v, Ord v) => Constructible (HashSet v) where
    empty = HashSet.empty
    insert v m = HashSet.insert v m
    singleton v = HashSet.singleton v
    fromList = HashSet.fromList

instance Collapseable (HashSet v) where
    c_foldMap = foldMap
    elements = HashSet.toList
    size = HashSet.size
    isEmpty = HashSet.null
    toList = HashSet.toList

instance (Eq v, Hashable v) => Intersectable (HashSet v) where
    union a b = HashSet.union a b
    intersection a b = HashSet.intersection a b

instance (Hashable v, Ord v) => Container (HashSet v) where
    contains m v = HashSet.member v m

----------------------------------------------------------------------------------------------------

instance BaseContainer (TreeMap k v) where
    type ElemT (TreeMap k v) = (k,v)

instance (Ord k) => Constructible (TreeMap k v) where
    empty = DataMap.empty
    insert (k,v) m = DataMap.insert k v m
    singleton (k,v) = DataMap.singleton k v
    fromList = DataMap.fromList

instance Collapseable (TreeMap k v) where
    c_foldMap f m = DataMap.foldWithKey f' mempty m where  -- Handle map as collection of (k,v) pairs
        f' k v a = f (k,v) `mappend` a
    elements = DataMap.toList
    size = DataMap.size
    isEmpty = DataMap.null
    toList = DataMap.toList

instance (Ord k) => Intersectable (TreeMap k v) where
    union a b = DataMap.union a b
    intersection a b = DataMap.intersection a b

instance (Ord k, Eq v) => Container (TreeMap k v) where
    contains m (k,v) = case DataMap.lookup k m of
        Nothing -> False
        Just v2 -> v == v2

instance (Ord k) => HasLookup (TreeMap k v) where
    type KeyT (TreeMap k v) = k
    type ValueT (TreeMap k v) = v
    lookup k m = DataMap.lookup k m
    isMemberOf = DataMap.member

instance (Ord k) => MapLike (TreeMap k v) where
    mapInsert k v m = DataMap.insert k v m
    mapDelete k m = DataMap.delete k m
    keys m = DataMap.keys m
    values m = DataMap.elems m
    hasKey = flip DataMap.member

instance MapFoldable DataMap.Map where
    mapWithKey = DataMap.mapWithKey
    foldrWithKey = DataMap.foldrWithKey
    foldlWithKey = DataMap.foldlWithKey

----------------------------------------------------------------------------------------------------

instance BaseContainer (HashMap k v) where
    type ElemT (HashMap k v) = (k,v)

instance (Hashable k, Ord k) => Constructible (HashMap k v) where
    empty = HashMap.empty
    insert (k,v) m = HashMap.insert k v m
    singleton (k,v) = HashMap.singleton k v
    fromList = HashMap.fromList

instance Collapseable (HashMap k v) where
    c_foldMap f m = -- Handle map as collection of (k,v) pairs
        HashMap.foldrWithKey f' mempty m where
        f' k v a = f (k,v) `mappend` a
    elements = HashMap.toList
    size = HashMap.size
    isEmpty = HashMap.null
    toList = HashMap.toList

instance (Eq k, Hashable k) => Intersectable (HashMap k v) where
    union a b = HashMap.union a b
    intersection a b = HashMap.intersection a b

instance (Hashable k, Ord k, Eq v) => Container (HashMap k v) where
    contains m (k,v) = case HashMap.lookup k m of
        Nothing -> False
        Just v2 -> v == v2

instance (Hashable k, Ord k) => HasLookup (HashMap k v) where
    type KeyT (HashMap k v) = k
    type ValueT (HashMap k v) = v
    lookup k m = HashMap.lookup k m
    isMemberOf = HashMap.member

instance (Hashable k, Ord k) => MapLike (HashMap k v) where
    mapInsert k v m = HashMap.insert k v m
    mapDelete k m = HashMap.delete k m
    keys m = HashMap.keys m
    values m = HashMap.elems m
    hasKey = flip HashMap.member

instance MapFoldable HashMap.HashMap where
    mapWithKey = HashMap.mapWithKey
    foldrWithKey = HashMap.foldrWithKey
    foldlWithKey = HashMap.foldlWithKey'

{-# LANGUAGE TupleSections #-}

module Data.VectorClock (
        -- * Vector clock type
        VectorClock,
        -- * Construction
        empty, singleton, fromList,
        -- * Query
        null, size, member, lookup, toList,
        -- * Insertion
        insert, inc, incWithDefault,
        -- * Deletion
        delete,
        -- * Merges
        combine, max,
        -- * Relations
        Relation(..), relation, causes,
        -- * Debugging
        valid
    ) where

import Prelude hiding ( null, lookup, max )
import qualified Prelude

import Data.Binary ( Binary(..) )
import Data.List ( foldl', sort, nub )
import Data.Maybe ( isJust, catMaybes )

-- | A vector clock is, conceptually, an associtive list sorted by the
-- value of the key, where each key appears only once.
data VectorClock a b = VectorClock { clock :: [(a, b)] }
                       deriving ( Eq )

instance (Show a, Show b) => Show (VectorClock a b) where
    show vc = show (clock vc)

instance (Binary a, Binary b) => Binary (VectorClock a b) where
    put vc = put (clock vc)
    get = get >>= \xys -> return (VectorClock { clock = xys })

-- | The relations two vector clocks may find themselves in.
data Relation = Causes | CausedBy | Concurrent
                deriving (Eq, Show)

-- | The empty vector clock.
empty :: VectorClock a b
empty = VectorClock { clock = [] }

-- | A vector clock with a single element.
singleton :: a -> b -> VectorClock a b
singleton x y = VectorClock { clock = [(x, y)] }

-- | Insert each entry in the list one at a time.
fromList :: (Ord a) => [(a, b)] -> VectorClock a b
fromList = foldl' (\vc (x, y) -> insert x y vc) empty

-- | All the entries in the vector clock.  Note that this is /not/ the
-- inverse of 'fromList'.
toList :: VectorClock a b -> [(a, b)]
toList = clock

-- | Is the vector clock empty?
null :: VectorClock a b -> Bool
null = Prelude.null . clock

-- | The number of entries in the vector clock.
size :: VectorClock a b -> Int
size = length . clock

-- | Lookup the value for a key in the vector clock and remove the
-- corresponding entry.
extract :: (Ord a) => a -> VectorClock a b -> (Maybe b, VectorClock a b)
extract x vc =
    case span (\(x', _) -> x' < x) (clock vc) of
      (_, []) ->
          (Nothing, vc)
      (xys, xys'@((x', y') : xys'')) ->
          if x' == x
          then (return y', vc { clock = xys ++ xys'' })
          else (Nothing, vc { clock = xys ++ xys' })

-- | Lookup the value for a key in the vector clock.
lookup :: (Ord a) => a -> VectorClock a b -> Maybe b
lookup x = fst . extract x

-- | Is the given key a key in an entry of the vector clock?
member :: (Ord a) => a -> VectorClock a b -> Bool
member x = isJust . lookup x

-- | Delete an entry from the vector clock.  If the requested entry
-- does not exist, does nothing.
delete :: (Ord a) => a -> VectorClock a b -> VectorClock a b
delete x = snd . extract x

-- | Insert or replace the entry for a key.
insert :: (Ord a) => a -> b -> VectorClock a b -> VectorClock a b
insert x y vc = vc { clock = go (clock vc) }
  where
    go [] = [(x, y)]
    go (xy@(x', _) : xys)
        | x' < x    = xy : go xys
        | x' == x   = (x, y) : xys
        | otherwise = (x, y) : xy : xys

-- | Increment the entry for a key.
inc :: (Ord a, Num b) => a -> VectorClock a b -> Maybe (VectorClock a b)
inc x vc = lookup x vc >>= \y -> return (insert x (y + fromInteger 1) vc)

-- | Increment the entry for a key.  If the key does not exist, assume
-- it was the default.
incWithDefault :: (Ord a, Num b)
               => a -> VectorClock a b -> b -> VectorClock a b
incWithDefault x vc y' =
    case lookup x vc of
      Nothing -> insert x (y' + fromInteger 1) vc
      Just y  -> insert x (y + fromInteger 1) vc

-- | Combine two vector clocks entry-by-entry.
combine :: (Ord a, Ord b)
        => (a -> Maybe b -> Maybe b -> Maybe b)
        -> VectorClock a b
        -> VectorClock a b
        -> VectorClock a b
combine f vc1 vc2 =
    VectorClock { clock = catMaybes (go (clock vc1) (clock vc2)) }
  where
    go [] xys = map (\(x, y) -> (x ~^ f x Nothing (Just y))) xys
    go xys [] = map (\(x, y) -> (x ~^ f x (Just y) Nothing)) xys
    go (xy@(x, y) : xys) (xy'@(x', y') : xys')
        | x < x'     = (x ~^ f x (Just y) Nothing) : go xys (xy' : xys')
        | x == x'    = (x ~^ f x (Just y) (Just y')) : go xys xys'
        | otherwise  = (x' ~^ f x' Nothing (Just y')) : go (xy : xys) xys'

    (~^) x v = v >>= return . (x,)

-- | The maximum of the two vector clocks.
max :: (Ord a, Ord b) => VectorClock a b -> VectorClock a b -> VectorClock a b
max = combine maxEntry
  where
    maxEntry _ Nothing Nothing    = Nothing
    maxEntry _ x@(Just _) Nothing = x
    maxEntry _ Nothing y@(Just _) = y
    maxEntry _ (Just x) (Just y)  = Just (Prelude.max x y)

-- | The relation between the two vector clocks.
relation :: (Ord a, Ord b) => VectorClock a b -> VectorClock a b -> Relation
relation vc1 vc2 = go (clock vc1) (clock vc2)
  where
    go ((x, y) : xys) ((x', y') : xys')
        | x == x'   =
            if y == y'
            then go xys xys'
            else if y < y'
                 then if checkCauses xys xys' then Causes else Concurrent
                 else if checkCauses xys' xys then CausedBy else Concurrent
        | otherwise = Concurrent
    go _ _ = Concurrent

    checkCauses [] [] = True
    checkCauses ((x, y) : xys) ((x', y') : xys')
        | x == x'   = if y <= y' then checkCauses xys xys' else False
        | otherwise = False
    checkCauses _ _ = False

-- | Short-hand for @relation vc1 vc2 == Causes@.
causes :: (Ord a, Ord b) => VectorClock a b -> VectorClock a b -> Bool
causes vc1 vc2 = relation vc1 vc2 == Causes

-- | Check whether the vector clock is valid or not.
valid :: (Ord a, Ord b) => VectorClock a b -> Bool
valid vc = let xys = clock vc
               xysSorted = sort xys
               xysNub = nub xys
           in xys == xysSorted && xys == xysNub

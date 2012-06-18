{-# LANGUAGE TupleSections #-}

module Data.VectorClock (
        -- * Vector clock type
        VectorClock,
        -- * Construction
        empty, singleton, fromList,
        -- * Query
        null, size, member, lookup,
        -- * Insertion
        insert,
        -- * Delete
        delete,
        -- * Merges
        combine, max,
        -- * Relations
        relation,
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
      (_, [])                    -> (Nothing, vc)
      (xys, xys'@((x', y') : _)) -> ( if x' == x then Just y' else Nothing
                                    , vc { clock = xys ++ xys' } )

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
insert x y vc =
    let xys' = go (clock vc)
    in vc { clock = reverse xys' }
  where
    go [] = []
    go (xy@(x', _) : xys)
        | x' < x    = xy : go xys
        | x' == x   = (x, y) : xys
        | otherwise = xy : xys

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
    go [] _ = Concurrent
    go _ [] = Concurrent
    go (xy@(x, y) : xys) (xy'@(x', y') : xys')
        | x < x'    = go xys (xy' : xys')
        | x == x'   = (if y < y' then checkCausedBy else checkCauses) xys xys'
        | otherwise = go (xy : xys) xys'

    checkCauses _ [] = Causes
    checkCauses [] _ = Causes
    checkCauses (xy@(x, y) : xys) (xy'@(x', y') : xys')
        | x < x'    = checkCauses xys (xy' : xys')
        | x == x'   = if y > y' then checkCauses xys xys' else Concurrent
        | otherwise = checkCauses (xy : xys) xys'

    checkCausedBy _ [] = CausedBy
    checkCausedBy [] _ = CausedBy
    checkCausedBy (xy@(x, y) : xys) (xy'@(x', y') : xys')
        | x < x'    = checkCauses xys (xy' : xys')
        | x == x'   = if y < y' then checkCauses xys xys' else Concurrent
        | otherwise = checkCauses (xy : xys) xys'

-- | Check whether the vector clock is valid or not.
valid :: (Ord a, Ord b) => VectorClock a b -> Bool
valid vc = let xys = clock vc
               xysSorted = sort xys
               xysNub = nub xys
           in xys == xysSorted && xys == xysNub

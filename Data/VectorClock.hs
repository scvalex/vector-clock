module Data.VectorClock (
        -- * Vector clock type
        VectorClock,
        -- * Construction
        empty, singleton,
        -- * Query
        null, size, member, lookup,
        -- * Insertion
        insert,
        -- * Delete
        delete
    ) where

import Prelude hiding ( null, lookup )
import qualified Prelude

import Data.Maybe ( isJust )

-- | A vector clock is, conceptually, an associtive list sorted by the
-- value of the key, where each key appears only once.
data (Ord a, Ord b) => VectorClock a b = VectorClock { clock :: [(a, b)] }

-- | The empty vector clock.
empty :: (Ord a, Ord b) => VectorClock a b
empty = VectorClock { clock = [] }

-- | A vector clock with a single element.
singleton :: (Ord a, Ord b) => a -> b -> VectorClock a b
singleton x y = VectorClock { clock = [(x, y)] }

-- | Is the vector clock empty?
null :: (Ord a, Ord b) => VectorClock a b -> Bool
null = Prelude.null . clock

-- | The number of entries in the vector clock.
size :: (Ord a, Ord b) => VectorClock a b -> Int
size = length . clock

-- | Lookup the value for a key in the vector clock and remove the
-- corresponding entry.
extract :: (Ord a, Ord b) => a -> VectorClock a b -> (Maybe b, VectorClock a b)
extract x vc =
    case span (\(x', _) -> x' < x) (clock vc) of
      (_, [])                    -> (Nothing, vc)
      (xys, xys'@((x', y') : _)) -> ( if x' == x then Just y' else Nothing
                                    , vc { clock = xys ++ xys' } )

-- | Lookup the value for a key in the vector clock.
lookup :: (Ord a, Ord b) => a -> VectorClock a b -> Maybe b
lookup x = fst . extract x

-- | Is the given key a key in an entry of the vector clock?
member :: (Ord a, Ord b) => a -> VectorClock a b -> Bool
member x = isJust . lookup x

-- | Delete an entry from the vector clock.  If the requested entry
-- does not exist, does nothing.
delete :: (Ord a, Ord b) => a -> VectorClock a b -> VectorClock a b
delete x = snd . extract x

-- | Insert or replace the entry for a key.
insert :: (Ord a, Ord b) => a -> b -> VectorClock a b -> VectorClock a b
insert x y vc =
    let xys' = go (clock vc)
    in vc { clock = reverse xys' }
  where
    go [] = []
    go (xy@(x', _) : xys)
        | x' < x    = xy : go xys
        | x' == x   = (x, y) : xys
        | otherwise = xy : xys

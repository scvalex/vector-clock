module Data.VectorClock (
        -- * Vector clock type
        VectorClock,
        -- * Construction
        empty, singleton,
        -- * Query
        null, size, member, lookup
    ) where

import Prelude hiding ( null, lookup )
import qualified Prelude

import Data.Maybe ( isJust )

data (Eq a, Ord b) => VectorClock a b = VectorClock { clock :: [(a, b)] }

-- | The empty vector clock.
empty :: (Eq a, Ord b) => VectorClock a b
empty = VectorClock { clock = [] }

-- | A vector clock with a single element.
singleton :: (Eq a, Ord b) => a -> b -> VectorClock a b
singleton x y = VectorClock { clock = [(x, y)] }

-- | Is the vector clock empty?
null :: (Eq a, Ord b) => VectorClock a b -> Bool
null = Prelude.null . clock

-- | The number of entries in the vector clock.
size :: (Eq a, Ord b) => VectorClock a b -> Int
size = length . clock

-- | Is the given key a key in an entry of the vector clock?
member :: (Eq a, Ord b) => a -> VectorClock a b -> Bool
member x = isJust . Prelude.lookup x . clock

-- | Lookup the value for a key in the vector clock.
lookup :: (Eq a, Ord b) => a -> VectorClock a b -> Maybe b
lookup x = Prelude.lookup x . clock

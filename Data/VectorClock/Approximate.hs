{-# LANGUAGE Trustworthy #-}

-- | An approximate vector clock implementation in terms of
-- "Data.VectorClock.Simple".

module Data.VectorClock.Approximate (
        -- * Vector clock type
        VectorClock,
        -- * Construction
        empty, singleton, fromList,
        -- * Insertion
        insert
    ) where

import Data.Binary ( Binary(..) )
import Data.Hashable ( Hashable(..) )
import Data.List ( foldl' )

import qualified Data.VectorClock.Simple as VC

-- | An approximate vector clock is a normal vector clock, but several
-- keys are mapped to the same value.  This can lead to /false
-- positive/ 'relation's.  In other words, the fact that one vector
-- clock causes another is no longer enough information to say that
-- the one messages causes the other.  That said, experimental results
-- show that approximate vector clocks have good results in practice.
-- See the paper by R. Baldoni and M. Raynal for details.
data VectorClock a b = VectorClock
    { clock :: VC.VectorClock Int b
    , size  :: Int
    }

instance (Eq b) => Eq (VectorClock a b) where
    vc1 == vc2 = clock vc1 == clock vc2

instance (Show b) => Show (VectorClock a b) where
    show = show . clock

instance (Binary b) => Binary (VectorClock a b) where
    put vc = do
        put (clock vc)
        put (size vc)
    get = do
        xys <- get
        k <- get
        return (VectorClock { clock = xys, size = k })

-- | /O(1)/.  The empty vector clock.
empty :: Int                    -- ^ /size/: the maximum number of
                                -- entries in the vector clock
      -> VectorClock a b
empty k = VectorClock { clock = VC.empty, size = k }

-- | /O(N)/.  Insert each entry in the list one at a time.
fromList :: (Hashable a)
         => Int                 -- ^ /size/: the maximum number of
                                -- entries in the vector clock
         -> [(a, b)]            -- ^ /entries/: the entries to insert
                                -- in the newly created vector clock
         -> VectorClock a b
fromList k = foldl' (\vc (x, y) -> insert x y vc) (empty k)

-- | /O(1)/.  A vector clock with a single element.
singleton :: (Hashable a)
          => Int                -- ^ /size/: the maximum number of
                                -- entries in the vector clock
          -> a                  -- ^ /key/: the key for the entry
          -> b                  -- ^ /value/: the value for the entry
          -> VectorClock a b
singleton k x y = fromList k [(x, y)]

-- | /O(N)/.  Insert or replace the entry for a key.
insert :: (Hashable a)
       => a                     -- ^ /key/: the key for the entry
       -> b                     -- ^ /value/: the value for the entry
       -> VectorClock a b       -- ^ /vc/: the vector clock in which
                                -- to insert
       -> VectorClock a b
insert x y vc@(VectorClock { clock = xys, size = k }) =
    let xys' = VC.insert (hash x `mod` k) y xys in
    vc { clock = xys' }

{-# LANGUAGE Trustworthy #-}

-- | An approximate vector clock implementation in terms of
-- "Data.VectorClock.Simple".

module Data.VectorClock.Approximate (
        -- * Vector clock type
        VectorClock,
        -- * Construction
        empty, singleton, fromList,
        -- * Query
        null, size, member, lookup,
        -- * Insertion
        insert, inc, incWithDefault,
        -- * Deletion
        delete
    ) where

import Prelude hiding ( null, lookup )

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
    { vcClock :: VC.VectorClock Int b
    , vcSize  :: Int
    }

instance (Eq b) => Eq (VectorClock a b) where
    vc1 == vc2 = vcClock vc1 == vcClock vc2

instance (Show b) => Show (VectorClock a b) where
    show = show . vcClock

instance (Binary b) => Binary (VectorClock a b) where
    put vc = do
        put (vcClock vc)
        put (vcSize vc)
    get = do
        xys <- get
        k <- get
        return (VectorClock { vcClock = xys, vcSize = k })

-- | /O(1)/.  The empty vector clock.
empty :: Int                    -- ^ /size/: the maximum number of
                                -- entries in the vector clock
      -> VectorClock a b
empty k = VectorClock { vcClock = VC.empty, vcSize = k }

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

-- | /O(1)/.  Is the vector clock empty?
null :: VectorClock a b -> Bool
null = VC.null . vcClock

-- | /O(N)/.  The number of entries in the vector clock.  Note that
-- this may be less than the /size/ at construction.
size :: VectorClock a b -> Int
size = VC.size . vcClock

-- | /O(N)/.  Is the given key a key in an entry of the vector clock?
member :: (Hashable a) => a -> VectorClock a b -> Bool
member x (VectorClock { vcClock = xys, vcSize = k }) =
    VC.member (mapKey x k) xys

-- | /O(N)/.  Lookup the value for a key in the vector clock.
lookup :: (Hashable a)  => a -> VectorClock a b -> Maybe b
lookup x (VectorClock { vcClock = xys, vcSize = k }) =
    VC.lookup (mapKey x k) xys

-- | /O(N)/.  Insert or replace the entry for a key.
insert :: (Hashable a) => a -> b -> VectorClock a b -> VectorClock a b
insert x y vc@(VectorClock { vcClock = xys, vcSize = k }) =
    let xys' = VC.insert (mapKey x k) y xys in
    vc { vcClock = xys' }

-- | /O(N)/.  Increment the entry for a key.
inc :: (Hashable a, Num b) => a -> VectorClock a b -> Maybe (VectorClock a b)
inc x vc@(VectorClock { vcClock = xys, vcSize = k }) = do
    xys' <- VC.inc (mapKey x k) xys
    return (vc { vcClock = xys' })

-- | /O(N)/.  Increment the entry for a key.  If the key does not
-- exist, assume it was the default.
incWithDefault :: (Hashable a, Num b)
               => a               -- ^ /key/: the key of the entry
               -> VectorClock a b -- ^ /vc/: the vector clock
               -> b               -- ^ /default/: if the key is not
                                  -- found, assume its value was the
                                  -- default and increment that
               -> VectorClock a b
incWithDefault x vc@(VectorClock { vcClock = xys, vcSize = k }) y' =
    let xys' = VC.incWithDefault (mapKey x k) xys y' in
    vc { vcClock = xys' }

-- | /O(N)/.  Delete an entry from the vector clock.  If the requested
-- entry does not exist, does nothing.
delete :: (Hashable a) => a -> VectorClock a b -> VectorClock a b
delete x vc@(VectorClock { vcClock = xys, vcSize = k }) =
    let xys' = VC.delete (mapKey x k) xys in
    vc { vcClock = xys' }

-- | Map a key into the domain of approximate keys.
mapKey :: (Hashable a) => a -> Int -> Int
mapKey x k = hash x `mod` k

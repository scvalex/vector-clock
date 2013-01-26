{-# LANGUAGE Safe, TupleSections, DeriveDataTypeable, DeriveGeneric #-}

-- | A vector clock implementation in terms of simply-linked lists.

module Data.VectorClock.Simple (
        -- * Usage example
        -- $example

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
        combine, max, diff,
        -- * Relations
        Relation(..), relation, causes,
        -- * Debugging
        valid
    ) where

import Prelude hiding ( null, lookup, max )
import qualified Prelude

import Control.Applicative ( (<$>) )
import Data.Binary ( Binary(..) )
import Data.Data ( Data, Typeable )
import Data.Foldable ( Foldable(..) )
import Data.Traversable ( Traversable(..) )
import Data.List ( sort, nub )
import Data.Maybe ( isJust, catMaybes )
import GHC.Generics ( Generic )

-- $example
--
-- To create a vector clock, start from 'empty' and 'insert' elements
-- into it.  As a shortcut, 'fromList' just inserts all the elements
-- in a list, in order.
--
-- > let vc = empty in
-- > let vc' = insert 'a' 1 vc in
-- > let vc'' = insert 'b' 2 vc in
-- > vc'' == fromList [('a', 1), ('b', 2)]
--
-- Note that, /for different keys/, the order of insertion does not
-- matter:
--
-- > fromList [('a', 1), ('b', 2)] == fromList [('b', 2), ('a', 1)]
--
-- Once you have a given vector clock, you can 'lookup' its fields,
-- check that keys are 'member's, or convert it back 'toList' form.
--
-- > lookup 'a' [('a', 1), ('b', 2)] == Just 1
-- > lookup 'c' [('a', 1), ('b', 2)] == Nothing
--
-- The main operations that you would do with a vector clcok are to
-- 'inc'crement the entry corresponding to the current process and to
-- update the process's vector clock with the 'max' of its and the
-- received message's clocks.
--
-- > inc 'a' [('a', 1), ('b', 2)] = Just [('a', 2), ('b', 2)]
-- > max [('a', 1), ('b', 2)] [('c', 3), ('b', 1)] == [('a', 1), ('b', 2), ('c' 3)]
--
-- Finally, upon receiving different messages, you may wish to
-- discover the 'relation'ship, if any, between them.  This
-- information could be useful in determining the correct order to
-- process the messages.
--
-- > relation (fromList [('a', 1), ('b', 2)]) (fromList [('a', 2), ('b', 2)]) == Causes
-- > relation (fromList [('a', 2), ('b', 2)]) (fromList [('a', 1), ('b', 2)]) == CausedBy
-- > relation (fromList [('a', 2), ('b', 2)]) (fromList [('a', 1), ('b', 3)]) == Concurrent
--
-- In order to send and receive vector clocks, they must first be
-- serialized.  For this purpose, there is a 'Binary' instance for
-- 'VectorClock'.  Additionally, there are 'Data', 'Typeable', and
-- 'Generic' instances, which allow packages such as @cereal@, and
-- @sexp@ to automatically generate serializers and deserializers.

-- | A vector clock is, conceptually, an associtive list sorted by the
-- value of the key, where each key appears only once.
data VectorClock a b = VectorClock { clock :: [(a, b)] }
                     deriving ( Eq, Data, Generic, Typeable )

instance (Show a, Show b) => Show (VectorClock a b) where
    show = show . clock

instance (Binary a, Binary b) => Binary (VectorClock a b) where
    put = put . clock
    get = get >>= \xys -> return (VectorClock { clock = xys })

instance Foldable (VectorClock a) where
    foldMap f = foldMap f . map snd . clock

instance Functor (VectorClock a) where
    fmap f vc = vc { clock = map (\(x, y) -> (x, f y)) (clock vc) }

instance Traversable (VectorClock a) where
    traverse f vc =
        let f' (x, y) = (x,) <$> f y in
        (\xys -> vc { clock = xys }) <$> traverse f' (clock vc)

-- | The relations two vector clocks may find themselves in.
data Relation = Causes | CausedBy | Concurrent
                deriving (Eq, Show)

-- | /O(1)/.  The empty vector clock.
empty :: VectorClock a b
empty = VectorClock { clock = [] }

-- | /O(1)/.  A vector clock with a single element.
singleton :: (Ord a) => a -> b -> VectorClock a b
singleton x y = fromList [(x, y)]

-- | /O(N)/.  Insert each entry in the list one at a time.
fromList :: (Ord a) => [(a, b)] -> VectorClock a b
fromList = foldl' (\vc (x, y) -> insert x y vc) empty

-- | /O(1)/.  All the entries in the vector clock.  Note that this is
-- /not/ the inverse of 'fromList'.
toList :: VectorClock a b -> [(a, b)]
toList = clock

-- | /O(1)/.  Is the vector clock empty?
null :: VectorClock a b -> Bool
null = Prelude.null . clock

-- | /O(N)/.  The number of entries in the vector clock.
size :: VectorClock a b -> Int
size = length . clock

-- | /O(N)/.  Lookup the value for a key in the vector clock and
-- remove the corresponding entry.
extract :: (Ord a) => a -> VectorClock a b -> (Maybe b, VectorClock a b)
extract x vc =
    case span (\(x', _) -> x' < x) (clock vc) of
      (_, []) ->
          (Nothing, vc)
      (xys, xys'@((x', y') : xys'')) ->
          if x' == x
          then (return y', vc { clock = xys ++ xys'' })
          else (Nothing, vc { clock = xys ++ xys' })

-- | /O(N)/.  Lookup the value for a key in the vector clock.
lookup :: (Ord a) => a -> VectorClock a b -> Maybe b
lookup x = fst . extract x

-- | /O(N)/.  Is the given key a key in an entry of the vector clock?
member :: (Ord a) => a -> VectorClock a b -> Bool
member x = isJust . lookup x

-- | /O(N)/.  Delete an entry from the vector clock.  If the requested
-- entry does not exist, does nothing.
delete :: (Ord a) => a -> VectorClock a b -> VectorClock a b
delete x = snd . extract x

-- | /O(N)/.  Insert or replace the entry for a key.
insert :: (Ord a) => a -> b -> VectorClock a b -> VectorClock a b
insert x y vc = vc { clock = go (clock vc) }
  where
    go [] = [(x, y)]
    go (xy@(x', _) : xys)
        | x' < x    = xy : go xys
        | x' == x   = (x, y) : xys
        | otherwise = (x, y) : xy : xys

-- | /O(N)/.  Increment the entry for a key.
inc :: (Ord a, Num b) => a -> VectorClock a b -> Maybe (VectorClock a b)
inc x vc = lookup x vc >>= \y -> return (insert x (y + fromInteger 1) vc)

-- | /O(N)/.  Increment the entry for a key.  If the key does not
-- exist, assume it was the default.
incWithDefault :: (Ord a, Num b)
               => a               -- ^ /key/: the key of the entry
               -> VectorClock a b -- ^ /vc/: the vector clock
               -> b               -- ^ /default/: if the key is not
                                  -- found, assume its value was the
                                  -- /default/ and increment that
               -> VectorClock a b
incWithDefault x vc y' =
    case lookup x vc of
      Nothing -> insert x (y' + fromInteger 1) vc
      Just y  -> insert x (y + fromInteger 1) vc

-- | /O(max(N, M))/.  Combine two vector clocks entry-by-entry.
combine :: (Ord a, Ord b)
        => (a -> Maybe b -> Maybe b -> Maybe b)
    -- ^ a function that takes the /key/, the value of the entry in
    -- the left hand vector clock, if it exists, the value in the
    -- right hand vector clock, if it exists, and, if it wishes to
    -- keep a value for this /key/ in the resulting vector clock,
    -- returns it.
        -> VectorClock a b      -- ^ /lhs/: the left hand vector clock
        -> VectorClock a b      -- ^ /rhs/: the right hand vector clock
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

-- | /O(max(N, M))/.  The maximum of the two vector clocks.
max :: (Ord a, Ord b) => VectorClock a b -> VectorClock a b -> VectorClock a b
max = combine maxEntry
  where
    maxEntry _ Nothing Nothing    = Nothing
    maxEntry _ x@(Just _) Nothing = x
    maxEntry _ Nothing y@(Just _) = y
    maxEntry _ (Just x) (Just y)  = Just (Prelude.max x y)

-- | /O(min(N, M))/.  The relation between the two vector clocks.
relation :: (Ord a, Ord b) => VectorClock a b -> VectorClock a b -> Relation
relation vc1 vc2 = go (clock vc1) (clock vc2)
  where
    go xys@((x, y) : xyt) xys'@((x', y') : xyt')
        | x == x' =
            if y == y'
            then go xyt xyt'
            else if y < y'
                 then if checkCauses xyt xyt' then Causes else Concurrent
                 else if checkCauses xyt' xyt then CausedBy else Concurrent
        | x < x' = if checkCauses xys' xyt then CausedBy else Concurrent
        | x > x' = if checkCauses xys xyt' then Causes else Concurrent
        | otherwise = Concurrent
    go [] _ = Causes
    go _ [] = CausedBy

    checkCauses xys@((x, y) : xyt) ((x', y') : xyt')
        | x == x'   = if y <= y' then checkCauses xyt xyt' else False
        | x < x'    = False
        | otherwise = checkCauses xys xyt'
    checkCauses [] _ = True
    checkCauses _ _  = False

-- | /O(min(N, M))/.  Short-hand for @relation vc1 vc2 == Causes@.
causes :: (Ord a, Ord b) => VectorClock a b -> VectorClock a b -> Bool
causes vc1 vc2 = relation vc1 vc2 == Causes

-- | /O(M)/.  If @vc2 `causes` vc1@, compute the smallest @vc3@
-- s.t. @max vc3 vc2 == vc1@.  Note that the /first/ parameter is the
-- newer vector clock.
diff :: (Ord a, Ord b)
     => VectorClock a b -> VectorClock a b -> Maybe (VectorClock a b)
diff vc1 vc2 =
    if vc1 == vc2 then Just (fromList []) else
    if vc2 `causes` vc1 then Just (combine diffOne vc1 vc2) else Nothing
  where
    diffOne _ Nothing  Nothing  = Nothing
    diffOne _ x        Nothing  = x
    diffOne _ (Just x) (Just y) = if x == y then Nothing else Just x
    diffOne _ Nothing  (Just _) = error "diff broken"

-- | /O(N)/.  Check whether the vector clock is valid or not.
valid :: (Ord a, Ord b) => VectorClock a b -> Bool
valid vc = let xys = clock vc
               xysSorted = sort xys
               xysNub = nub xys
           in xys == xysSorted && xys == xysNub

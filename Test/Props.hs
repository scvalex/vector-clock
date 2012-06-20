{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Main where

import Prelude hiding ( null, lookup, max )
import qualified Prelude

import Control.Applicative ( (<$>) )

import Data.Binary ( encode, decode )
import Data.Maybe ( fromJust )
import Data.Monoid
import Data.VectorClock

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

type VC = VectorClock Char Int

instance (Arbitrary a, Ord a, Arbitrary b) => Arbitrary (VectorClock a b) where
    arbitrary = arbitrary >>= return . fromList

newtype Mutation = Mutation Int
    deriving ( Show )

instance Arbitrary Mutation where
    arbitrary = Mutation <$> (choose (0, 100))

main :: IO ()
main = defaultMainWithOpts
       [ testCase "size"  testSize
       , testCase "size2" testSize2
       , testCase "member" testMember
       , testCase "lookup" testLookup
       , testCase "insert" testInsert
       , testCase "inc" testInc
       , testCase "delete" testDelete
       , testCase "combine" testCombine
       , testProperty "fromList" propFromList
       , testProperty "binaryId" propBinaryId
       , testProperty "maxNotCauses" propMaxNotCauses
       , testProperty "relationInverse" propRelationInverse
       , testProperty "maxCommutative" propMaxCommutative
       , testProperty "relationTransitive" propRelationTransitive
       ] opts
  where
    opts = mempty {
             ropt_test_options =
                 Just (mempty
                       { topt_maximum_generated_tests            = Just 100
                       , topt_maximum_unsuitable_generated_tests = Just 5000
                       })}

--------------------------------
-- Unit tests
--------------------------------

testSize :: Assertion
testSize = do
    null empty             @?= True
    null (singleton 'a' 1) @?= False

testSize2 :: Assertion
testSize2 = do
    size empty                           @?= 0
    size (singleton 'a' 1)               @?= 1
    size (fromList [('a', 1), ('b', 1)]) @?= 2

testMember :: Assertion
testMember = do
    member 'a' (fromList [('a', 1), ('b', 2)]) @?= True
    member 'c' (fromList [('a', 1), ('b', 2)]) @?= False

testLookup :: Assertion
testLookup = do
    lookup 'a' (fromList [('a', 1), ('b', 2)]) @?= Just 1
    lookup 'b' (fromList [('a', 1), ('b', 2)]) @?= Just 2
    lookup 'c' (fromList [('a', 1), ('b', 2)]) @?= Nothing

testInsert :: Assertion
testInsert = do
    insert 'b' 2 (insert 'a' 1 empty) @?= fromList [('a', 1), ('b', 2)]
    insert 'a' 1 (insert 'b' 2 empty) @?= fromList [('a', 1), ('b', 2)]
    insert 'b' 2 (insert 'a' 1 empty) @?= fromList [('b', 2), ('a', 1)]
    insert 'a' 1 (insert 'b' 2 empty) @?= fromList [('b', 2), ('a', 1)]
    insert 'a' 2 (insert 'a' 1 empty) @?= fromList [('a', 2)]

testInc :: Assertion
testInc = do
    let vc = fromList [('a', 1), ('b', 2)]
    inc 'a' vc @?= Just (fromList [('a', 2), ('b', 2)])
    inc 'a' (fromJust (inc 'a' vc)) @?= Just (fromList [('a', 3), ('b', 2)])
    inc 'b' vc @?= Just (fromList [('a', 1), ('b', 3)])
    inc 'c' vc @?= Nothing
    incWithDefault 'c' vc 0 @?= fromList [('c', 1), ('a', 1), ('b', 2)]
    incWithDefault 'a' vc 0 @?= fromList [('a', 2), ('b', 2)]

testDelete :: Assertion
testDelete = do
    let vc = fromList [('a', 1), ('b', 2)]
    delete 'a' vc @?= fromList [('b', 2)]
    delete 'b' vc @?= fromList [('a', 1)]
    delete 'b' (delete 'a' vc) @?= empty
    delete 'a' (delete 'b' vc) @?= empty
    delete 'c' vc @?= vc
    delete 'a' (empty :: VectorClock Char Int) @?= empty

testCombine :: Assertion
testCombine = do
    let vc1 = fromList [('a', 1), ('b', 2)]
    let vc2 = fromList [('c', 3), ('b', 4)]
    let first_vc = \_ x _ -> x
    let second_vc = \_ _ x -> x
    let neither_vc = \_ _ _ -> Nothing
    combine first_vc vc1 vc2 @?= vc1
    combine second_vc vc1 vc2 @?= vc2
    combine neither_vc vc1 vc2 @?= empty
    max empty vc1 @?= vc1
    max vc2 empty @?= vc2
    max vc1 vc2 @?= fromList [('a', 1), ('b', 4), ('c', 3)]

--------------------------------
-- QuickCheck properties
--------------------------------

propFromList :: VC -> Bool
propFromList vc = valid vc

propBinaryId :: VC -> Bool
propBinaryId vc = vc == decode (encode vc)

-- @max vc1 vc2@ does not cause either @vc1@ or @vc2@
propMaxNotCauses :: VC -> VC -> Bool
propMaxNotCauses vc1 vc2 =
    let vcMax = max vc1 vc2 in
    relation vcMax vc1 /= Causes &&
    relation vcMax vc2 /= Causes &&
    relation vc1 vcMax /= CausedBy &&
    relation vc2 vcMax /= CausedBy

-- | Increment the entries that correspond to the mutations.
applyMutations :: VC -> [Mutation] -> VC
applyMutations vc ms =
    let sources = map fst $ toList vc in
    let len = length sources in
    foldl (\vc' (Mutation m) ->
               incWithDefault (sources !! (m `mod` len)) vc' 0) vc ms

-- vc1 causes vc2 iff vc2 is caused by vc1
propRelationInverse :: VC -> [Mutation] -> Property
propRelationInverse vc1 ms =
    not (null vc1) ==>
    let vc2 = applyMutations vc1 ms in
    vc1 `causes` vc2 ==>
    relation vc2 vc1 == CausedBy

propMaxCommutative :: VC -> VC -> Bool
propMaxCommutative vc1 vc2 =
    max vc1 vc2 == max vc2 vc1

propRelationTransitive :: VC -> [Mutation] -> [Mutation] -> Property
propRelationTransitive vc1 ms1 ms2 =
    not (null vc1) ==>
    let vc2 = applyMutations vc1 ms1 in
    let vc3 = applyMutations vc2 ms2 in
    vc1 `causes` vc2 && vc2 `causes` vc3 ==>
    vc1 `causes` vc3

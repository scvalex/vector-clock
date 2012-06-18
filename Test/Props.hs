{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ( null )

import Data.Monoid
import Data.VectorClock
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.HUnit

default (Int)

main :: IO ()
main = defaultMainWithOpts
       [ testCase "size" test_size
       ] opts
  where
    opts = mempty {
             ropt_test_options =
                 Just $ mempty { topt_maximum_generated_tests = Just 500
                               , topt_maximum_unsuitable_generated_tests = Just 500
                               }
           }

test_size :: Assertion
test_size = do
  null empty             @?= True
  null (singleton 1 'a') @?= False

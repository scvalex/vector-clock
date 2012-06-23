{-# LANGUAGE Safe #-}

-- | A vector clock implementation.
--
-- This module re-exports "Data.VectorClock.Simple", which is the
-- fully-featured vector clock library.  If you wish to use
-- approximate vector clocks, which are significantly smaller and have
-- bounded size, but are not exact, use "Data.VectorClock.Approximate"
-- instead.
--
-- See @Fundamentals of Distributed Computing: A Practical Tour of
-- Vector Clock Systems@ by R. Baldoni and M. Raynal for an overview
-- of vector clocks.

module Data.VectorClock (
        module Data.VectorClock.Simple
    ) where

import Data.VectorClock.Simple

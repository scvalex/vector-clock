-- | A vector clock implementation.
--
-- This module re-exports "Data.VectorClock.Simple", which is the
-- fully-featured vector clock library.  As time goes on, other
-- modules may be added to this package to support the various
-- flavours of vector clocks useful to different applications.
--
-- See @Fundamentals of Distributed Computing: A Practical Tour of
-- Vector Clock Systems@ by R. Baldoni and M. Raynal for an overview
-- of vector clocks.

module Data.VectorClock (
        module Data.VectorClock.Simple
    ) where

import Data.VectorClock.Simple

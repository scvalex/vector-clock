module Data.VectorClock (
        VectorClock
    ) where

data VectorClock a b = VectorClock { clock :: [(a, b)] }

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Graphics.CadSim.Boolean where

class BooleanOps a b c | a b -> c where
    union :: a -> b -> c
    intersection :: a -> b -> c
    xor :: a -> b -> c


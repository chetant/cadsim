{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Graphics.CadSim.Boolean where

class BooleanOps a where
    union :: a -> a -> a
    intersection :: a -> a -> a
    xor :: a -> a -> a


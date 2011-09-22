{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Graphics.CadSim.Move where

class Moveable a b c | c->b where
    translate :: a -> b -> c
    scale :: a -> b -> c
    rotate :: a -> b -> c

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.CadSim.Move where

class Moveable a b where
    translate :: a -> b -> a
    scale :: a -> b -> a
    rotate :: a -> b -> a

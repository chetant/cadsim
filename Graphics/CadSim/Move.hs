{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.CadSim.Move where

class Moveable a b c where
    translate :: a -> b -> c
    scale :: a -> b -> c
    rotate :: a -> b -> c

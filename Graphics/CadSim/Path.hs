{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Graphics.CadSim.Path
(
 Point(..)
,Points(..)
,Face(..)
,Path(..)
,NumberConv(..)
) where

import GHC.Float(float2Double)

import Graphics.CadSim.Move
import Graphics.CadSim.Boolean

data Point = Point {
      pointX :: Double
    , pointY :: Double
    }

instance Show Point where
    show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

type Points = [Point]

class Path a where
    getExterior :: a -> Points
    getHoles :: a -> [Points]
    getHoles _ = []

data Face = Face {
      exterior :: Points
    , holes :: [Points]
    }

mapTuple f = map (\(x,y) -> Point (f x) (f y))

-- Type class to get around having dupe instances
class NumberConv a where
    numToDouble :: a -> Double

instance Integral a => NumberConv a where
    numToDouble = fromIntegral

instance NumberConv Double where
    numToDouble = id

instance NumberConv Float where
    numToDouble = float2Double

--------- Instances for Path -------
instance NumberConv a => Path [(a, a)] where
    getExterior = mapTuple numToDouble

instance Path Face where
    getExterior = exterior
    getHoles = holes

--------- Path Instance for Moveable and Boolean -------
instance (Path a) => Moveable a Point where
    translate path _ = path
    scale path _ = path
    rotate path _ = path

instance (Path a) => BooleanOps a where
    union a b = a
    intersection a b = a
    xor a b = a

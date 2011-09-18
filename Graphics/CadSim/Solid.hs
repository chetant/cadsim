{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}
module Graphics.CadSim.Solid
(
 Point(..)
,toPoint,toPointX,toPointY,toPointZ
,dist,distX,distY,distZ
,Points(..)
,Extents(..), getCenter
,Solid(..)
-- ,Object(..)
) where

import Data.List(foldl')
import Data.Convertible
import qualified Data.Vector.Unboxed as V

import Graphics.CadSim.Move
import Graphics.CadSim.Boolean
import qualified Graphics.CadSim.Path as Path

data Point = Point {
      pointX :: Double
    , pointY :: Double
    , pointZ :: Double
    } deriving Eq

instance (Real a, Real b, Real c) => Convertible (a, b, c) Point where
    safeConvert (x, y, z) = Right $ Point (realToFrac x) (realToFrac y) (realToFrac z)

type Points = [Point]
type Extents = (Point, Point)

instance Show Point where
    show (Point x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Moveable Point Point Point where
    translate (Point tx ty tz) (Point x y z) = Point (x+tx) (y+ty) (z+tz)
    scale     (Point sx sy sz) (Point x y z) = Point (x*sx) (y*sy) (z*sz)
    rotate _ path = undefined

instance Moveable Double Point Point where
    translate t (Point x y z) = Point (x+t) (y+t) (z+t)
    scale     s (Point x y z) = Point (x*s) (y*s) (z*s)
    rotate _ path = undefined

instance Moveable Point Extents Extents where
    translate t (p1, p2) = (translate t p1, translate t p2)
    scale     s (p1, p2) = (scale s p1, scale s p2)
    rotate _ path = undefined

getCenter :: Extents -> Point
getCenter ((Point x1 y1 z1), (Point x2 y2 z2)) = Point ((x2 + x1) / 2) ((y2 + y1) / 2) ((z2 + z1) / 2)

instance Num Point where
    a + b = a `translate` b
    a * b = a `scale` b
    a - b = a `translate` (negate b)
    negate a = (((-1) :: Double) `scale` a)
    abs (Point x y z) = Point (abs x) (abs y) (abs z)
    signum (Point x y z) = Point (signum x) (signum y) (signum z)
    fromInteger = toPoint

dist :: Point -> Point -> Double
dist (Point x1 y1 z1) (Point x2 y2 z2) = sqrt $ dx2 + dy2 + dz2
    where dx = x2 - x1
          dy = y2 - y1
          dz = z2 - z1
          dx2 = dx * dx
          dy2 = dy * dy
          dz2 = dz * dz

distX :: Point -> Point -> Double
distX (Point x1 _ _) (Point x2 _ _) = x2 - x1

distY :: Point -> Point -> Double
distY (Point _ y1 _) (Point _ y2 _) = y2 - y1

distZ :: Point -> Point -> Double
distZ (Point _ _ z1) (Point _ _ z2) = z2 - z1

toPoint :: (Real n) => n -> Point
toPoint n = Point n' n' n'
    where n' = realToFrac n

toPointX :: (Real n) => n -> Point
toPointX n = Point n' 0 0
    where n' = realToFrac n

toPointY :: (Real n) => n -> Point
toPointY n = Point 0  n' 0
    where n' = realToFrac n

toPointZ :: (Real n) => n -> Point
toPointZ n = Point 0  0  n'
    where n' = realToFrac n

class Solid a where
    getExterior :: a -> Points
    getExtents :: a -> (Point, Point)
    getExtents path = foldl' minMaxPt initPt points
        where points = getExterior path
              minMaxPt (Point minX minY minZ, Point maxX maxY maxZ) (Point x y z) = 
                  (Point (min minX x) (min minY y) (min minZ z)
                  ,Point (max maxX x) (max maxY y) (max maxZ z))
              firstPoint = take 1 points
              initPt
                  | null firstPoint = (Point 0 0 0, Point 0 0 0)
                  | otherwise = (head firstPoint, head firstPoint)

data Object = Object { 
      objPoints :: V.Vector (Double, Double, Double)
    , objIndices :: V.Vector Int
    }

newtype Radians = Radians Double

degrees :: Real a => a -> Radians
degrees deg = Radians $ (realToFrac deg * pi) / 360.0

radians :: Double -> Radians
radians = Radians

sweep :: (Path.Path a) => a -> Radians -> Object
sweep = 


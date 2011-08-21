{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}
module Graphics.CadSim.Path
(
 Point(..)
,toPoint,toPointX,toPointY
,dist,distX,distY
,Points(..)
,Extents(..), getCenter
,Face(..)
,Path(..)
) where

import GHC.Float(float2Double)
import Data.List(foldl')
import Data.Convertible

import Graphics.CadSim.Move
import Graphics.CadSim.Boolean

data Point = Point {
      pointX :: Double
    , pointY :: Double
    } deriving Eq

type Points = [Point]
type Extents = (Point, Point)

instance Show Point where
    show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Moveable Point Point Point where
    translate (Point tx ty) (Point x y) = Point (x+tx) (y+ty)
    scale     (Point sx sy) (Point x y) = Point (x*sx) (y*sy)
    rotate _ path = undefined

instance Moveable Point Extents Extents where
    translate t (p1, p2) = (translate t p1, translate t p2)
    scale     s (p1, p2) = (scale s p1, scale s p2)
    rotate _ path = undefined

getCenter :: Extents -> Point
getCenter ((Point x1 y1), (Point x2 y2)) = Point ((x2 + x1) / 2) ((y2 + y1) / 2)

instance Num Point where
    a + b = a `translate` b
    a * b = a `scale` b
    a - b = a `translate` (negate b)
    negate a = (toPoint ((-1) :: Double) `scale` a)
    abs (Point x y) = Point (abs x) (abs y)
    signum (Point x y) = Point (signum x) (signum y)
    fromInteger = toPoint

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt $ dx2 + dy2
    where dx = x2 - x1
          dy = y2 - y1
          dx2 = dx * dx
          dy2 = dy * dy

distX :: Point -> Point -> Double
distX (Point x1 _) (Point x2 _) = x2 - x1

distY :: Point -> Point -> Double
distY (Point _ y1) (Point _ y2) = y2 - y1

instance Convertible Double Double where
    safeConvert = Right

toPoint :: (Convertible n Double) => n -> Point
toPoint n = Point n' n'
    where n' = convert n

toPointX :: (Convertible n Double) => n -> Point
toPointX n = Point n' 0
    where n' = convert n

toPointY :: (Convertible n Double) => n -> Point
toPointY n = Point 0  n'
    where n' = convert n

class Path a where
    getExterior :: a -> Points
    getHoles :: a -> [Points]
    getHoles _ = []
    getExtents :: a -> (Point, Point)
    getExtents path = foldl' minMaxPt initPt points
        where points = getExterior path
              minMaxPt (Point minX minY, Point maxX maxY) (Point x y) = (Point (min minX x)
                                                                               (min minY y)
                                                                        ,Point (max maxX x)
                                                                               (max maxY y))
              firstPoint = take 1 points
              initPt
                  | null firstPoint = (Point 0 0, Point 0 0)
                  | otherwise = (head firstPoint, head firstPoint)

data Face = Face {
      exterior :: Points
    , holes :: [Points]
    }

mapTuple f = map (\(x,y) -> Point (f x) (f y))

--------- Instances for Path -------
instance Convertible a Double => Path [(a, a)] where
    getExterior = mapTuple convert

instance Path Face where
    getExterior = exterior
    getHoles = holes

--------- Path Instance for Moveable and Boolean -------
instance (Path a) => Moveable Point a Face where
    translate tp path = Face exterior holes
        where exterior = map (translate tp) $ getExterior path
              holes = map (map (translate tp)) $ getHoles path
    scale sp path = Face exterior holes
        where exterior = map (scale sp) $ getExterior path
              holes = map (map (scale sp)) $ getHoles path
    rotate _ path = undefined

instance (Path a) => BooleanOps a where
    union a b = a
    intersection a b = a
    xor a b = a

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
,getScaleFactor
) where

import GHC.Float(float2Double)
import Data.List(foldl')
import Data.Convertible
import Data.Word
import Data.Bits
import System.IO.Unsafe(unsafePerformIO)

import qualified Algebra.Clipper as C

import Graphics.CadSim.Move
import Graphics.CadSim.Boolean

data Point = Point {
      pointX :: Double
    , pointY :: Double
    } deriving Eq

instance (Real a, Real b) => Convertible (a, b) Point where
    safeConvert (x, y) = Right $ Point (realToFrac x) (realToFrac y)

type Points = [Point]
type Extents = (Point, Point)

instance Show Point where
    show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Moveable Point Point Point where
    translate (Point tx ty) (Point x y) = Point (x+tx) (y+ty)
    scale     (Point sx sy) (Point x y) = Point (x*sx) (y*sy)
    rotate _ path = undefined

instance Moveable Double Point Point where
    translate t (Point x y) = Point (x+t) (y+t)
    scale     s (Point x y) = Point (x*s) (y*s)
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
    negate a = (((-1) :: Double) `scale` a)
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

toPoint :: (Real n) => n -> Point
toPoint n = Point n' n'
    where n' = realToFrac n

toPointX :: (Real n) => n -> Point
toPointX n = Point n' 0
    where n' = realToFrac n

toPointY :: (Real n) => n -> Point
toPointY n = Point 0  n'
    where n' = realToFrac n

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

--------- Instances for Clipper Data types -------

instance Convertible C.IntPoint Point where
    safeConvert ip = Right $ Point (fromIntegral (C.pointX ip)) (fromIntegral (C.pointY ip))

instance Convertible Point C.IntPoint where
    safeConvert (Point x y) = Right $ C.IntPoint (round x) (round y)

-- instance Path C.Polygon where
--     getExterior = map convert . C.getPoints
--     getHoles _ = []

-- instance Path C.Polygons where
--     getExterior ps = if null polys then [] else (map convert . C.getPoints . head) polys
--         where polys = C.getPolys ps
--     getHoles ps = map (map convert . C.getPoints) . tail $ polys
--         where polys = C.getPolys ps

instance (Path a) => Convertible a (Double, C.Polygon) where
    safeConvert path = Right $ (sFactor, C.Polygon pts)
        where path' :: Face
              path' = sFactor `scale` path
              pts = map convert $ getExterior path'
              sFactor = getScaleFactor path

instance Convertible (Double, C.Polygon) Face where
    safeConvert (sFactor, C.Polygon pts) = Right $ Face exterior []
        where exterior = map (sc . convert) pts
              sc :: Point -> Point
              sc = scale (1/sFactor)

instance (Path a) => Convertible a (Double, C.Polygons) where
    safeConvert path = Right $ (sFactor, convertByScale sFactor path)
        where sFactor = getScaleFactor path

convertByScale :: (Path a) => Double -> a -> C.Polygons
convertByScale sFactor path = C.Polygons (exterior:holes)
        where path' :: Face
              path' = sFactor `scale` path
              exterior = C.Polygon $ map convert $ getExterior path'
              holes = map (C.Polygon . map convert) $ getHoles path'

instance Convertible (Double, C.Polygons) Face where
    safeConvert (sFactor, C.Polygons pts) = Right $ Face (unFace exterior) (map unFace holes)
        where (exterior:holes) = map (convert . ((,) sFactor)) pts
              unFace (Face a _) = a

instance Convertible C.Polygon Face where
    safeConvert ps = Right $ Face exterior []
        where exterior = (map convert . C.getPoints) ps

instance Convertible C.Polygons Face where
    safeConvert ps = Right $ Face exterior holes
        where exterior = if null polys then [] else (map convert . C.getPoints . head) polys
              holes = map (map convert . C.getPoints) . tail $ polys
              polys = C.getPolys ps

-- instance Convertible Point C.IntPoint where
--     safeConvert (Point x y) = Right $ C.IntPoint (round x) (round y)

--------- Path Instance for Moveable and Boolean -------
instance (Path a) => Moveable Point a Face where
    translate tp path = Face exterior holes
        where exterior = map (translate tp) $ getExterior path
              holes = map (map (translate tp)) $ getHoles path
    scale sp path = Face exterior holes
        where exterior = map (scale sp) $ getExterior path
              holes = map (map (scale sp)) $ getHoles path
    rotate _ path = undefined

instance (Path a) => Moveable Double a Face where
    translate tp path = Face exterior holes
        where exterior = map (translate tp) $ getExterior path
              holes = map (map (translate tp)) $ getHoles path
    scale sp path = Face exterior holes
        where exterior = map (scale sp) $ getExterior path
              holes = map (map (scale sp)) $ getHoles path
    rotate _ path = undefined

getScaleFactor :: (Path a) => a -> Double
getScaleFactor path = fromIntegral $ 2 ^ (32 - maxDigits 63)
    where allPoints = getExterior path ++ concat (getHoles path)
          maxInPt m (Point x y) = max y $ max m x
          maxPoint :: Word64
          maxPoint = truncate $ foldl' maxInPt 0 allPoints
          maxDigits :: Int -> Int
          maxDigits i = case ((maxPoint `shift` (-i)) .&. 0x1) of
                          1 -> (i+1)
                          _ -> maxDigits (i-1)

doubleConvert :: (Path a, Path b) => a -> b -> (Double, C.Polygons, C.Polygons)
doubleConvert a b = if sFa > sFb 
                    then (sFa, convertByScale sFa a, convertByScale sFa b)
                    else (sFb, convertByScale sFb a, convertByScale sFb b)
    where sFa = getScaleFactor a
          sFb = getScaleFactor b

instance (Path a, Path b) => BooleanOps a b Face where
    union a b = 
        case doubleConvert a b of
          (sFactor, a', b') -> convert (sFactor, unsafePerformIO $ a' `C.union` b')
    intersection a b = 
        case doubleConvert a b of
          (sFactor, a', b') -> convert (sFactor, unsafePerformIO $ a' `C.intersection` b')
    xor a b = 
        case doubleConvert a b of
          (sFactor, a', b') -> convert (sFactor, unsafePerformIO $ a' `C.xor` b')

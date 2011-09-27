{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}
module Graphics.CadSim.Solid
(
 Point(..)
,toPoint,toPointX,toPointY,toPointZ
,dist,distX,distY,distZ
,Points(..)
,Extents(..), getCenter
,Solid(..)
,Object(..)
,join, extrude, sweep
) where

import Data.List(foldl')
import Control.Monad(foldM)
import System.IO.Unsafe(unsafePerformIO)
import Data.Maybe(fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Convertible
import Data.Tensor(Vertex3(..))
import Graphics.Rendering.OpenGL.Raw.Core31(GLdouble)
import Graphics.Rendering.OpenGL.GL.VertexSpec(Normal3(..))
import Graphics.Rendering.OpenGL.GLU.Tessellation
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

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

instance Moveable Point Point where
    translate (Point x y z) (Point tx ty tz)  = Point (x+tx) (y+ty) (z+tz)
    scale     (Point x y z) (Point sx sy sz) = Point (x*sx) (y*sy) (z*sz)
    rotate pt _ = undefined

instance Moveable Point Double where
    translate (Point x y z) t = Point (x+t) (y+t) (z+t)
    scale     (Point x y z) s = Point (x*s) (y*s) (z*s)
    rotate pt _ = undefined

instance (Real a, Real b, Real c) => Moveable Point (a,b,c) where
    translate (Point x y z) (tx, ty, tz) = Point (x + realToFrac tx) 
                                                 (y + realToFrac ty)
                                                 (z + realToFrac tz)
    scale     (Point x y z) (sx, sy, sz) = Point (x * realToFrac sx)
                                                 (y * realToFrac sy)
                                                 (z * realToFrac sz)
    rotate pt _ = undefined

instance Moveable Extents Point where
    translate (p1, p2) t = (translate t p1, translate t p2)
    scale     (p1, p2) s = (scale s p1, scale s p2)
    rotate pt _ = undefined

getCenter :: Extents -> Point
getCenter ((Point x1 y1 z1), (Point x2 y2 z2)) = Point ((x2 + x1) / 2) ((y2 + y1) / 2) ((z2 + z1) / 2)

instance Num Point where
    a + b = a `translate` b
    a * b = a `scale` b
    a - b = a `translate` (negate b)
    negate a = scale a ((-1) :: Double)
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
    getVertices :: a -> Points
    getTris :: a -> [(Point, Point, Point)]
    getExtents :: a -> (Point, Point)
    getExtents path = foldl' minMaxPt initPt points
        where points = getVertices path
              minMaxPt (Point minX minY minZ, Point maxX maxY maxZ) (Point x y z) = 
                  (Point (min minX x) (min minY y) (min minZ z)
                  ,Point (max maxX x) (max maxY y) (max maxZ z))
              firstPoint = take 1 points
              initPt
                  | null firstPoint = (Point 0 0 0, Point 0 0 0)
                  | otherwise = (head firstPoint, head firstPoint)

data Object = Object { 
      objPoints :: V.Vector (Double, Double, Double) -- Points with (x, y, z)
    , objTris :: V.Vector (Int, Int, Int) -- Triangles indexing edges (a, b, c)
    } deriving (Show)

fromTuple (x, y, z) = Point x y z
toTuple (Point x y z) = (x, y, z)

instance Moveable Object Point where
    translate obj pt = obj {objPoints = V.map (toTuple . flip translate pt . fromTuple) (objPoints obj)}
    scale     obj pt = obj {objPoints = V.map (toTuple . flip scale pt . fromTuple) (objPoints obj)}
    rotate    obj _ = undefined

instance Moveable Object Double where
    translate obj t = obj { objPoints = V.map (toTuple . flip translate t . fromTuple) (objPoints obj) }
    scale     obj s = obj { objPoints = V.map (toTuple . flip scale s . fromTuple)     (objPoints obj) }
    rotate    obj _ = undefined

instance (Real a, Real b, Real c) => Moveable Object (a,b,c) where
    translate obj pt = obj {objPoints = V.map (toTuple . flip translate pt . fromTuple) (objPoints obj)}
    scale     obj pt = obj {objPoints = V.map (toTuple . flip scale pt . fromTuple) (objPoints obj)}
    rotate    obj _ = undefined

instance Solid Object where
    getVertices = map fromTuple . V.toList . objPoints
    getTris obj = map mkTri $ V.toList $ objTris obj
        where vs = objPoints obj
              mkTri (iv1, iv2, iv3) = (fromTuple $ vs V.! iv1, 
                                       fromTuple $ vs V.! iv2, 
                                       fromTuple $ vs V.! iv3)

newtype Radians = Radians Double

join :: Object -> Object -> Object
join o1 o2 = Object vs tris
    where numvs = V.length $ objPoints o1
          vs = objPoints o1 V.++ objPoints o2
          tris = objTris o1 V.++ (V.map (\(v1, v2, v3) -> (v1+numvs,v2+numvs,v3+numvs)) $ objTris o2)

degrees :: Real a => a -> Radians
degrees deg = Radians $ (realToFrac deg * pi) / 360.0

radians :: Double -> Radians
radians = Radians

sweep :: (Path.Path a) => a -> Radians -> Object
sweep = undefined

instance Convertible Path.Point Point where
    safeConvert (Path.Point x y) = Right (Point x y 0)

extrude :: (Path.Path a) => a -> Double -> Object
extrude path len = foldl' join s1 (s2:sides)
  where s1 = unsafePerformIO $ tesselate path
        s2 = translate s1 (toPointZ len)
        sides = map mkSides loops
        loops :: [V.Vector (Double, Double, Double)]
        loops = map (V.fromList . map (toTuple . convert)) $ 
                (Path.getExterior path) : (Path.getHoles path)
        mkSides :: V.Vector (Double, Double, Double) -> Object
        mkSides pts = Object (pts V.++ pts') sides
            where numPts = V.length pts
                  nPt i | i == (numPts-1) = 0
                        | otherwise = i+1
                  mkSide i = [(i, j, k), (i, k, l)]
                      where j = nPt i
                            k = j + numPts
                            l = i + numPts
                  sides = V.fromList $ concatMap mkSide [0..(numPts-1)]
                  pts' = V.map (\(x, y, z)->(x, y, (z+len))) pts

tesselate :: (Path.Path a) => a -> IO Object
tesselate path = do
  let pt2Vtx (Path.Point x y) = AnnotatedVertex (Vertex3 (realToFrac x) (realToFrac y) 0) 0
      pts2Contour = ComplexContour . map pt2Vtx
      exterior = pts2Contour $ Path.getExterior path
      holes = map pts2Contour $ Path.getHoles path
      normal :: Normal3 GLdouble
      normal = Normal3 0 0 1
      combine :: Combiner Int
      combine _ _ = 0
      cpoly = ComplexPolygon (exterior:holes)
      vtx2Pt (AnnotatedVertex (Vertex3 x y z) _) = (realToFrac x, realToFrac y, realToFrac z)
      getPtsFromTri (Triangle v1 v2 v3) = [vtx2Pt v1, vtx2Pt v2, vtx2Pt v3]
      getAllPts ts = concatMap getPtsFromTri ts
  (Triangulation tris) <- triangulate TessWindingOdd 1E-6 normal combine cpoly
  let pts = getAllPts tris
      points = Set.toList . Set.fromList $ pts
      ptMap = Map.fromList $ zip points [0..]
      getIdx v = fromJust $ Map.lookup v ptMap
      getTris ts [] = ts
      getTris ts (v1:v2:v3:vs) = let v1' = getIdx v1
                                     v2' = getIdx v2
                                     v3' = getIdx v3
                                     t = (v1', v2', v3')
                                 in getTris (t:ts) vs
      triangles = getTris [] pts
  return $ Object (V.fromList points) (V.fromList triangles)

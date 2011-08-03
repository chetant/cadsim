module Graphics.CadSim.Core
(
 Point2
,Point2List
,Point3
,Point3List
,Units
,RotUnit
) where

data Point2 = Point2 {
      point2X :: Double
    , point2Y :: Double
    }

newtype Point2List = P2List [Point2]

data Point3 = Point3 {
      point3X :: Double
    , point3Y :: Double
    , point3Z :: Double
    }

newtype Point3List = P3List [Point3]

data Units = UnitsMM
           | UnitsInch

newtype RotUnit = RotUnit Double

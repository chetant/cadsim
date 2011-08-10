module Graphics.CadSim.Path_ where

data Point = Point {
      pointX :: Double
    , pointY :: Double
    }

type Points = [Point]

class Path a where
    getExterior :: a -> Points
    getHoles :: a -> [Points]
    getHoles _ = []


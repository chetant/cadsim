{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Graphics.CadSim.Path
(
 Point(..)
,Points(..)
,Face(..)
) where

import Graphics.CadSim.Move
import Graphics.CadSim.Boolean
import Graphics.CadSim.Render

import Graphics.CadSim.Path_
import Graphics.CadSim.Path.Render

data Face = Face {
      exterior :: Points
    , holes :: [Points]
    }

--------- Instances for Path -------
instance Path [(Int, Int)] where
    getExterior = map (\(x,y) -> Point (fromIntegral x) (fromIntegral y))

instance Path [(Double, Double)] where
    getExterior = map (uncurry Point)

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

--------- Path Instance for Renderable -------
instance (Path a) => Renderable a where
    render = render_


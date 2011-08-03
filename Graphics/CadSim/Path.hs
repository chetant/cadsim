module Graphics.CadSim.Path
(
 OpenPath
,ClosedPath
) where

import Graphics.CadSim.Core

newtype OpenPath = OpenPath Point2List
newtype ClosedPath = ClosedPath Point2List

module Main where

-- import Algebra.Clipper
import Graphics.CadSim.Render
import Graphics.CadSim.Path
import Graphics.CadSim.Path.Render
import Graphics.CadSim.Boolean

main = do
  -- let p1 = Polygon [IntPoint 0 0, IntPoint 100 0, IntPoint 100 100, IntPoint 0 100]
  --     p2 = Polygon [IntPoint 0 0, IntPoint 100 0, IntPoint 50 200]
  -- soln <- (Polygons [p1]) `intersection` (Polygons [p2])
  -- area <- polygonArea p1
  -- clk <- polygonIsClockwise p1
  -- putStrLn $ "Intersection:" ++ show soln
  -- putStrLn $ "Area:" ++ show area
  -- putStrLn $ "Is Clockwise?:" ++ show clk

  let a :: [(Double,Double)]
      a = [(0,0)
          ,(0,1)
          ,(1,1)
          ,(1,0)
          ]
      b :: [(Double,Double)]
      b = [(0,0)
          ,(1,0)
          ,(0.5,2)
          ]
  print $ getScaleFactor a
  print $ getScaleFactor b
  let soln :: Face
      soln = b `intersection` a
  print $ getExtents a
  print $ getExtents b
  print $ getExtents soln
  render soln

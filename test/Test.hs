module Main where

import Graphics.CadSim.Render
import Graphics.CadSim.Path
import Graphics.CadSim.Path.Render
import Graphics.CadSim.Path.Monad
import Graphics.CadSim.Boolean
import Graphics.CadSim.Move
import Graphics.CadSim.Solid hiding (toPointX)
import Graphics.CadSim.Solid.Render

a = do
  right 1
  up 1
  left 1

b = do
  right 1
  move ((-0.5), 2)

main = do
  let r1 = rectangle 20 10
      r2 = square 5 `translate` toPointX 7.5
      r2' = square 5 `translate` (0, 7.5)
      r3 = square 5 `translate` toPointX (-7.5)
      s2 = r1 `xor` r2 `xor` r3
      s1 = getPath b `intersection` getPath a
      obj = extrude s2 30
      obj' = sweep s1 (degrees 360)
  --render s1
  render obj'

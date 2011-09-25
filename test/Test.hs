module Main where

import Graphics.CadSim.Render
import Graphics.CadSim.Path
import Graphics.CadSim.Path.Render
import Graphics.CadSim.Path.Monad
import Graphics.CadSim.Boolean
import Graphics.CadSim.Move
import Graphics.CadSim.Solid(extrude)
import Graphics.CadSim.Solid.Render

testb = do
  moveTo (0,0)
  right 1
  move ((-0.5), 2)
  close

main = do
  let r1 = rectangle 20 10
      r2 = square 5 `translate` toPointX 7.5
      r2' = square 5 `translate` (0, 7.5)
      r3 = square 5 `translate` toPointX (-7.5)
      s2 = r1 `xor` r2 `xor` r3
      obj = extrude s2 30
  render obj

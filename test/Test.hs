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
      b' = getPath testb
  let soln = b' `intersection` a
      r1 = rectangle 20 10
      r2 :: Face
      r2 = toPointX 7.5 `translate` square 5
      r3 :: Face
      r3 = toPointX (-7.5) `translate` square 5
      s2 = (r1 `xor` r2) `xor` r3
      obj = extrude soln 5
  -- print obj
  render $ obj
  -- render s2

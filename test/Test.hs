module Main where

import Graphics.CadSim.Render
import Graphics.CadSim.Path
import Graphics.CadSim.Path.Render
import Graphics.CadSim.Boolean

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
  let soln = b `intersection` a
  render soln

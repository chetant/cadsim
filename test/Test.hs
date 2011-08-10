module Main where

import Graphics.CadSim.Render
import Graphics.CadSim.Path
import Graphics.CadSim.Path.Render

main = do
  let a :: [(Int,Int)]
      a = [(0,0)
          ,(0,1)
          ,(1,1)
          ,(1,0)
          ]
  print $ getExterior a
  render a

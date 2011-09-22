{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Graphics.CadSim.Solid.Render
(
render_
) where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL hiding(Point, translate)
import qualified Graphics.Rendering.OpenGL as GL
import Data.Bits((.|.))
import System.Exit(exitWith, ExitCode(..))
import Control.Monad(when, forever)

import Graphics.CadSim.Solid
import Graphics.CadSim.Move
import Graphics.CadSim.Render

instance Renderable Object where
    render = render_
initGL :: IO ()
initGL = do
  shadeModel $= Smooth -- enables smooth color shading
  clearColor $= Color4 0 0 0 0 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc $= Just Lequal
  hint PerspectiveCorrection $= Nicest
  lineSmooth $= Enabled
  hint LineSmooth $= Nicest
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

resizeScene :: (Point, Point) -> GLFW.WindowSizeCallback
resizeScene ps w     0      = resizeScene ps w 1 -- prevent divide by zero
resizeScene e@(minPt, maxPt) width height = do
  return ()
  let Point dx dy _ = maxPt - minPt
      Point cx cy _ = getCenter e
      hdx = dx / 2
      hdy = dy / 2
      aScr = fromIntegral width / fromIntegral height
      aPth = if dy == 0 then dx else (dx / dy)
      (hw, hh) = if aPth > aScr then (hdx, hdx / aScr) else (hdy*aScr, hdy)
  viewport $= ((Position 0 0), (Size (fromIntegral width) (fromIntegral height)))
  matrixMode $= Projection
  loadIdentity
  ortho2D (realToFrac (cx - hw)) (realToFrac (cx + hw)) (realToFrac (cy - hh)) (realToFrac (cy + hh))
  matrixMode $= (Modelview 0)
  loadIdentity
  flush

maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

drawOrigin = do
  let max = 1000000 -- maxNonInfiniteFloat undefined
  lineStipple $= Just (1, 0x00FF)
  lineWidth $= 0.5
  renderPrimitive Lines $ do
               -- x axis
               vertex $ vert (-max) 0 0
               vertex $ vert max 0 0
               -- y axis
               vertex $ vert 0 (-max) 0
               vertex $ vert 0 max 0
  lineStipple $= Nothing
  lineWidth $= 1

point :: (Real a) => a -> a -> a -> Vector3 GLdouble
point x y z = Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

vert :: (Real a) => a -> a -> a -> Vertex3 GLdouble
vert x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

drawObj :: Bool -> Object -> IO ()
drawObj drawOriginAxes obj = do
  -- clear the screen and the depth bufer
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  GL.translate $ point 0 0 (-0.5)

  when drawOriginAxes drawOrigin

  lineWidth $= 2
  let mkVert (Point x y z) = vertex (vert x y z)
  renderPrimitive Triangles $ mapM_ (\(v1, v2, v3) -> mkVert v1 >> mkVert v2 >> mkVert v3) $ getTris obj
  lineWidth $= 1
  flush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = shutdown >> return ()
keyPressed _           _    = return ()

render_ :: Object -> IO ()
render_ o = do
     True <- GLFW.initialize
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     let dspOpts = GLFW.defaultDisplayOptions
                     -- get a 800 x 600 window
                     { GLFW.displayOptions_width  = 800
                     , GLFW.displayOptions_height = 600
                     -- Set depth buffering and RGBA colors
                     , GLFW.displayOptions_numRedBits   = 8
                     , GLFW.displayOptions_numGreenBits = 8
                     , GLFW.displayOptions_numBlueBits  = 8
                     , GLFW.displayOptions_numAlphaBits = 8
                     , GLFW.displayOptions_numDepthBits = 1
                     -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
                     }
         renderFunc = drawObj True o
     -- open a window
     True <- GLFW.openWindow dspOpts
     -- window starts at upper left corner of the screen
     GLFW.setWindowPosition 0 0
     GLFW.setWindowTitle "CadSim"
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback renderFunc
     -- register the function called when our window is resized
     let scaleFactor = 0.1
         size = scaleFactor * (max (uncurry distX extents) (uncurry distY extents))
         extents@(p1, p2) = getExtents o
         extents' = (toPoint (-size) + p1, toPoint size + p2)
     GLFW.setWindowSizeCallback (resizeScene extents')
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback keyPressed
     GLFW.setWindowCloseCallback shutdown
     -- initialize our window.
     initGL
     -- start event processing engine
     forever $ do
       renderFunc
       GLFW.swapBuffers

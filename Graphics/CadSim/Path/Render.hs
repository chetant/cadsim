{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Graphics.CadSim.Path.Render
(
render_
) where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw(gluOrtho2D)
import Data.Bits((.|.))
import System.Exit(exitWith, ExitCode(..))
import Control.Monad(forever)

import Graphics.CadSim.Path
import Graphics.CadSim.Move
import Graphics.CadSim.Render

instance (Path a) => Renderable a where
    render = render_

initGL :: IO ()
initGL = do
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL  -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

resizeScene :: (Point, Point) -> GLFW.WindowSizeCallback
resizeScene ps w     0      = resizeScene ps w 1 -- prevent divide by zero
resizeScene (minPt, maxPt) width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  -- gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  gluOrtho2D (toGlX minPt) (toGlX maxPt) (toGlY minPt) (toGlY maxPt)
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: IO ()
drawScene = do
  -- clear the screen and the depth bufer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef 0 0 (-0.5) --Move left 1.5 Units and into the screen 6.0
  
  -- draw a triangle
  glBegin gl_TRIANGLES
  glVertex3f 0      1  0 -- top
  glVertex3f 1    (-1) 0 -- bottom right
  glVertex3f (-1) (-1) 0 -- bottom left
  glEnd

  glTranslatef 3 0 0  -- move right three units

  glBegin gl_QUADS
  glVertex3f (-1)   1  0 -- top left
  glVertex3f   1    1  0 -- top right
  glVertex3f   1  (-1) 0 -- bottom right
  glVertex3f (-1) (-1) 0 -- bottom left
  glEnd
  
  glFlush

toGlX = realToFrac . pointX
toGlY = realToFrac . pointY
toGl = realToFrac

drawPath :: (Point, Point) -> Path a => a -> IO ()
drawPath (minPt, maxPt) path = do
  
  -- clear the screen and the depth bufer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef 0 0 (-0.5) --Move left 1.5 Units and into the screen 6.0

  let drawPath_ ps = do
          glBegin gl_LINE_LOOP
          mapM_ (\(Point x y) -> glVertex3f (toGl x) (toGl y) 0) ps
          glEnd
  drawPath_ $ getExterior path
  mapM_ drawPath_ $ getHoles path
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = shutdown >> return ()
keyPressed _           _    = return ()

render_ :: Path a => a -> IO ()
render_ p = do
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
     -- open a window
     True <- GLFW.openWindow dspOpts
     -- window starts at upper left corner of the screen
     GLFW.setWindowPosition 0 0
     GLFW.setWindowTitle "CadSim"
     -- register the function to do all our OpenGL drawing
     GLFW.setWindowRefreshCallback drawScene
     -- register the funciton called when our window is resized
     let scaleFactor = 0.1
         size = scaleFactor * (max (uncurry distX extents) (uncurry distY extents))
         extents@(p1, p2) = getExtents p
         extents' = (toPoint (-size) `translate` p1, toPoint size `translate` p2)
     print size
     print extents'
     GLFW.setWindowSizeCallback (resizeScene extents')
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback keyPressed
     GLFW.setWindowCloseCallback shutdown
     -- initialize our window.
     initGL
     -- start event processing engine
     forever $ do
       drawPath extents p
       GLFW.swapBuffers

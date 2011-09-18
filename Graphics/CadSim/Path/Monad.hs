{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances #-}
module Graphics.CadSim.Path.Monad
(
getPath, up, down, left, right, close, move, moveTo
) where

import Data.Convertible
import Control.Monad.State

import Graphics.CadSim.Path

--------- Monad for Path building -------
data PBuilder = PBuilder {
      currPointer :: Point
    , currPath :: Points
    , addCurrPath :: Points -> Face -> Face
    , builderFace :: Face
    }

type PathBuild a = State PBuilder a

setExterior_ ps face = face { exterior = ps }

-- |Execute the monad and get the Face (Path instance)
getPath :: PathBuild a -> Face
getPath m = builderFace $ execState m initState
    where initState = PBuilder (Point 0 0) [] setExterior_ (Face [] [])

-- |Select the exterior to work on
onExterior :: PathBuild ()
onExterior = modify (\(PBuilder xy p _ f) -> PBuilder xy p setExterior_ f)

-- |Select the hole to work on
onHole :: PathBuild ()
onHole = modify (\(PBuilder xy p _ f) -> PBuilder xy p setHole f)
    where setHole ps face = face { holes = (ps:holes face) }

-- |Moves pointer to location
moveTo :: (Convertible a Point) => a -> PathBuild ()
moveTo xy = modify (\pb -> pb { currPointer = convert xy })

-- |Moves pointer to relative location, drawing a line
move :: (Convertible a Point) => a -> PathBuild ()
move xy = modify (\pb -> let cp = currPointer pb
                             xy' = convert xy + cp
                             path = currPath pb
                         in
                         pb { currPointer = xy'
                            , currPath = xy':cp:path})

-- |Moves pointer left, drawing a line
left :: (Real a) => a -> PathBuild ()
left x = modify (\pb -> let xy = currPointer pb
                            xy' = toPointX (-x) + xy
                        in
                        pb { currPointer = xy'
                           , currPath = xy':xy:currPath pb})

-- |Moves pointer right, drawing a line
right :: (Real a) => a -> PathBuild ()
right x = modify (\pb -> let xy = currPointer pb
                             xy' = toPointX x + xy
                         in
                         pb { currPointer = xy'
                            , currPath = xy':xy:currPath pb})

-- |Moves pointer up, drawing a line
up :: (Real a) => a -> PathBuild ()
up y = modify (\pb -> let xy = currPointer pb
                          xy' = toPointY y + xy 
                      in
                      pb { currPointer = xy'
                         , currPath = xy':xy:currPath pb})

-- |Moves pointer down, drawing a line
down :: (Real a) => a -> PathBuild ()
down y = modify (\pb -> let xy = currPointer pb 
                            xy' = toPointY (-y) + xy
                        in
                        pb { currPointer = xy'
                           , currPath = xy':xy:currPath pb})

-- |Close the current path, start a new one
close :: PathBuild()
close = modify (\pb -> let face = builderFace pb
                           addPath = addCurrPath pb
                           ps = currPath pb
                       in  pb { currPath = [], builderFace = addPath ps face })


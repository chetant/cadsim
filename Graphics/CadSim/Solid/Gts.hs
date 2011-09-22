{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Graphics.CadSim.Solid.Gts where

import Foreign.Ptr(Ptr, nullPtr)
import qualified Bindings.Gts as Gts

type GtsSurface = Ptr Gts.C'GtsSurface




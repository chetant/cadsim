module Graphics.CadSim.Render where

class Renderable a where
    render :: a -> IO ()

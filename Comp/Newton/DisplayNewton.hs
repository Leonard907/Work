import Graphics.UI.GLUT
import Data.IORef
import NewtonFractal


main :: IO ()
main = do   
  getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered, RGBMode]
  initialWindowSize $= Size 800 800
  state <- newIORef 0
  createWindow "Julia Set"
  displayCallback $= displayNewton 
  mainLoop

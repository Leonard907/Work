import Graphics.UI.GLUT
import Data.IORef
import Mandelbrot


main :: IO ()
main = do   
  getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered, RGBMode]
  initialWindowSize $= Size 800 800
  createWindow "Mandelbrot Set"
  displayCallback $= displayMandelbrot
  mainLoop
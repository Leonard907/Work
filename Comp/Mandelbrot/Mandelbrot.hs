module Mandelbrot (displayMandelbrot, idle) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT 
import Data.IORef
import Data.List
import Control.Monad
import Codec.Picture

-- Regions of complex numbers
minX, maxX, minY, maxY :: Float
minX = -2
maxX = 2
minY = -2
maxY = 2

-- canvas setup
width, height :: Int
width = 800
height = 800

-- max iterations considered
maxI :: Int
maxI = 50

-- Next state and Iterations
next :: (Float,Float) -> (Float,Float) -> (Float,Float)
next (xF,yF) (x, y) = (x*x - y*y + xF, 2*x*y + yF)

-- Take initial value
iter :: (Float,Float) -> [(Float,Float)]
iter c = take maxI $ iterate (next c) c

-- test for convergence
iterations :: [(Float,Float)] -> Int
iterations cs = length $ takeWhile (\(x,y) -> x^2 + y^2 <= 27) cs

iterationTime :: [(Float,Float)] -> [Int]
iterationTime = map (iterations . iter) 

isBounded :: [(Float,Float)] -> Bool
isBounded cs = iterations cs == length cs

-- mapping coordinate to the display area
mapN :: Float -> Int -> Int -> Float -> Float -> Float
mapN x rangeStart rangeEnd minN maxN = 
    (x - (fromIntegral rangeStart)) / (fromIntegral (rangeEnd - rangeStart)) 
    * (maxN - minN) + minN

-- list of points to be start with
mapPoints :: [(Float,Float)]
mapPoints = [(mapN (fromIntegral x) 0 width minX maxX, mapN (fromIntegral y) 0 height minY maxY) | 
          x <- [0..(width-1)], y <- [0..(height-1)]] 

-- max iterations for each point
parameters :: [((Float,Float), Int)]
parameters = zip [(x,y) | x <- [1..800], y <- [1..800]] (map (iterations . iter) mapPoints)

-- coloring
coloring :: [(Float,Float,Float)]
coloring = map (\(x,y) -> lit (x,y)) parameters
  where
    lit (x,y) 
      | y == maxI = (0.0, 0.0, 0.0)
      | otherwise = (((fromIntegral 2) * (bright y) / 255), ((bright y) / 255), ((0.5 * (bright y)) / 255))
    bright n = mapN (sqrt (mapN (fromIntegral n) 0 maxI 0 1)) 0 1 0 255

-- change color overtime
changedColoring :: Int -> [(Float,Float,Float)]
changedColoring n = 
  map (\(x,y,z) -> (x,
                    y * ((fromIntegral n) * 0.1 + (fromIntegral 1)),
                    z)) coloring

transformColoring :: [[(Float,Float,Float)]]
transformColoring = [changedColoring n | n <- [1..10]] ++ 
                  [changedColoring n | n <- (reverse [1..10])]

-- dense
dense :: [(Float,Float)] -> [(Float,Float)]
dense p = map (\(x,y) -> (x / 2, y / 2)) p

-- combining result
coloredPoints =  [filter (\(x,y) -> x /= (0.0,0.0,0.0)) $ 
              zip (transformColoring !! n) (dense mapPoints)| n <- [0..20]]

-- convert to vertex and color type
vertex2f :: (GLfloat, GLfloat) -> IO ()
vertex2f (x, y) = vertex $ Vertex2 x y 

color3f :: (GLfloat,GLfloat,GLfloat) -> IO ()
color3f (x,y,z) = color $ Color3 x y z

-- Reshaping window
reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

-- display function
displayMandelbrot :: IORef GLfloat -> DisplayCallback
displayMandelbrot state = do    
    clear [ColorBuffer]
    s <- get state
    mandelbrot s      
    swapBuffers

-- For color changing animation
idle :: IORef GLfloat -> IdleCallback
idle state = do
  state $~! (+ 1)
  postRedisplay Nothing

mandelbrot :: GLfloat -> IO ()
mandelbrot n = renderPrimitive Points $ 
       forM_ (coloredPoints !! (mod (floor n) 20)) $ \(c,p) -> do
          color3f c
          vertex2f p 
   
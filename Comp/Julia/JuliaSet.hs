module JuliaSet (displayJulia) where

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
    
    -- Julia constants
    c1, c2, c3 :: (Float,Float)
    c1 = (0.8 * (f (-1)), 0.156)
    c2 = (0.285,0.01)
    c3 = (0.70716 * (f (-1)), 0.3842 * (f (-1)))

    -- canvas setup
    width, height :: Int
    width = 800
    height = 800
   
    -- shorter version for "fromIntegral"
    f :: Int -> Float
    f = fromIntegral
    
    -- max iterations considered
    maxI :: Int
    maxI = 50
    
    -- Next state and Iterations
    next :: (Float,Float) -> (Float,Float) -> (Float,Float)
    next (xF,yF) (x, y) = (x*x - y*y + xF, 2*x*y + yF)
    
    -- Take initial value
    iter :: (Float,Float) -> [(Float,Float)]
    iter c = take maxI $ iterate (next c) c
    
    juliaIter :: (Float,Float) -> [(Float,Float)]
    juliaIter c = take maxI $ iterate (next c1) c
    
    -- test for convergence
    iterations :: [(Float,Float)] -> Int
    iterations cs = length $ takeWhile (\(x,y) -> x^2 + y^2 <= 27) cs
    
    iterationTime :: [(Float,Float)] -> [Int]
    iterationTime = map (iterations . iter) 
    
    -- mapping coordinate to the display area
    mapN :: Float -> Int -> Int -> Float -> Float -> Float
    mapN x rangeStart rangeEnd minN maxN = 
        (x - (f rangeStart)) / (f (rangeEnd - rangeStart)) 
        * (maxN - minN) + minN
    
    -- list of points to be start with
    mapPoints :: [(Float,Float)]
    mapPoints = [(mapN (f x) 0 width minX maxX, mapN (f y) 0 height minY maxY) | 
              x <- [0..(width-1)], y <- [0..(height-1)]] 
    
    -- max iterations for each point
    parameters :: [((Float,Float), Int)]
    parameters = zip [(x,y) | x <- [1..800], y <- [1..800]] 
               (map (iterations . juliaIter) mapPoints)

    -- coloring
    coloring :: [(Float,Float,Float)]
    coloring = map (\(x,y) -> lit (x,y)) parameters
      where
        lit (x,y) 
          | y == maxI = (0.0, 0.0, 0.0)
          | y <= ((maxI `div` 5) * 4) && y >= ((maxI `div` 5) * 3) = 
                   ((3.0 * (b y) / 255), ((b y) / 255), ((b y) / 255))
          | y <= ((maxI `div` 5) * 3) && y >= ((maxI `div` 5) * 2) = 
                   (((b y) / 255), ((b y) / 255), (2.0 * (b y) / 255))
          | y <= ((maxI `div` 5) * 2) && y >= ((maxI `div` 5) * 1) = 
                   ((2.0 * (b y) / 255), (1.5 * (b y) / 255), (0.5 * (b y) / 255))       
          | otherwise = 
                   ((0.5 * (b y) / 255), (2 * (b y) / 255), (0.5 * (b y) / 255))
        b n = mapN (sqrt (mapN (f n) 0 maxI 0 1)) 0 1 0 255
    
    -- dense
    dense :: [(Float,Float)] -> [(Float,Float)]
    dense p = map (\(x,y) -> (x / 2, y / 2)) p

    -- combining result
    coloredPoints = filter (\(x,y) -> x /= (0.0,0.0,0.0)) $ 
                  zip coloring (dense mapPoints)     
                
    coloredSquares = filter (\(x,y) -> x /= (0.0,0.0,0.0)) $
                   zip coloring (toSquare (dense (mapPoints)))
        where 
            toSquare ps = map vertices ps
            vertices (x,y) = [(x,y,0),(x,y-0.003,0),(x-0.003,y-0.003,0),(x-0.003,y,0)]
    
    -- convert to vertex and color type
    vertex3f :: (GLfloat, GLfloat,GLfloat) -> IO ()
    vertex3f (x, y, z) = vertex $ Vertex3 x y z

    
    
    color3f :: (GLfloat,GLfloat,GLfloat) -> IO ()
    color3f (x,y,z) = color $ Color3 x y z
    
    -- display function
    displayJulia :: DisplayCallback
    displayJulia = do    
        clear [ColorBuffer]
        julia      
        flush
    
    julia :: IO ()
    julia = renderPrimitive Quads $ 
           forM_ coloredSquares $ \(c,p) -> do
                 color3f c
                 mapM_ vertex3f p
                 
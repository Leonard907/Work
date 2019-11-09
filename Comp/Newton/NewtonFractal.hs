module NewtonFractal where

    import qualified Graphics.Rendering.OpenGL as GL
    import Graphics.UI.GLUT 
    import Data.IORef
    import Data.List
    import Control.Monad
    import Data.Complex
    
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
   
    -- shorter version for "fromIntegral"
    f :: Int -> Float
    f = fromIntegral
    
    -- max iterations considered
    maxI :: Int
    maxI = 50

    -- function used
    g :: (RealFloat a) => Complex a -> Complex a
    g x = x^8 + 15 * x^4 - 16

    g' :: (RealFloat a) => Complex a -> Complex a
    g' x = 7 * x^7 + 60 * x^3

    n_r x = x - ((1 :+ 0) * (g x) / (g' x))
    
    -- Next state and Iterations 
    -- (Stop after maxI is reached or the value doesn't converge anymore)
    next :: Int -> (Float,Float) -> (Int, (Float,Float))
    next iter (x,y)
        | n_r (tF x y) == (tF x y)  = (iter, (x,y)) -- The root
        | iter == maxI            = (maxI, (0.0,0.0)) -- No root
        | otherwise               = next (iter + 1) (sep (n_r (tF x y)))
        where tF re im = re :+ im
              sep (re :+ im) = (re,im)
    
    iterationTime :: (Int,(Float,Float)) -> Int
    iterationTime = (\(x,y) -> x)
    
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
    parameters :: [(Int,(Float,Float))]
    parameters = zip (map (iterationTime . next 0) mapPoints) 
               [(x,y) | x <- [1..800], y <- [1..800]]

    -- coloring
    coloring :: [(Float,Float,Float)]
    coloring = map (\(y,x) -> lit (y,x)) parameters
      where
        lit (y,x) 
          | y == maxI = (0.0, 0.0, 0.0)
          | y <= ((maxI `div` 5) * 4) && y >= ((maxI `div` 5) * 3) = 
                   ((2.0 * (b y) / 255), ((b y) / 255), ((b y) / 255))
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
    
    -- convert to vertex and color type
    vertex2f :: (GLfloat, GLfloat) -> IO ()
    vertex2f (x, y) = vertex $ Vertex2 x y 
    
    color3f :: (GLfloat,GLfloat,GLfloat) -> IO ()
    color3f (x,y,z) = color $ Color3 x y z
    
    -- display function
    displayNewton :: DisplayCallback
    displayNewton = do    
        clear [ColorBuffer]
        newton     
        flush
    
    newton :: IO ()
    newton = renderPrimitive Points $ 
           forM_ coloredPoints $ \(c,p) -> do
                 color3f c
                 vertex2f p
       
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL as SDL
import qualified SDL
import qualified SDL.Video.OpenGL as SDL
import System.Exit (exitFailure)

currentTime :: IO Instant
currentTime = Instant . realToFrac <$> getPOSIXTime

newtype Interval = Interval {interval :: Double}

newtype Instant = Instant Double

data Vec2 = Vec2 !Double !Double

instance Show Vec2 where
  show (Vec2 x y) = "(" ++ showDouble x ++ ", " ++ showDouble y ++ ")"

instance Show Object where
  show Object {objId, objPosition} = show objId ++ "@" ++ show objPosition

instance Show World where
  show World {worldObjects, worldAge} =
    showDouble worldAge ++ "s: " ++ show (length worldObjects) ++ " objects"

showDouble :: Double -> String
showDouble d = show $ fromIntegral (floor (d * (10.0 ^ precision))) / (10.0 ^ precision)
  where
    precision = 0

(.+.) :: Vec2 -> Vec2 -> Vec2
Vec2 x1 y1 .+. Vec2 x2 y2 = Vec2 (x1 + x2) (y1 + y2)

(*.) :: Double -> Vec2 -> Vec2
k *. Vec2 x y = Vec2 (k * x) (k * y)

dot :: Vec2 -> Vec2 -> Double
Vec2 x1 y1 `dot` Vec2 x2 y2 = (x1 * x2) + (y1 * y2)

distanceSquared :: Vec2 -> Vec2 -> Double
Vec2 x1 y1 `distanceSquared` Vec2 x2 y2 = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

zero :: Vec2
zero = Vec2 0.0 0.0

vecLength :: Vec2 -> Double
vecLength v = sqrt (distanceSquared v zero)

normalize :: Vec2 -> Vec2
normalize v = (1.0 / vl') *. v
  where
    vl = vecLength v
    vl' = if vl < 0.01 then 0.01 else vl

reflect :: Vec2 -> Vec2 -> Vec2
reflect d n = d .+. (((-2.0) * (d `dot` n')) *. n')
  where
    n' = normalize n

data Object = Object
  { objId :: !Int,
    objRadius :: !Double,
    objPosition :: !Vec2,
    objSpeed :: !Vec2,
    objColor :: !Color
  }

data Bound = Bound
  { boundId :: !Int,
    boundRadius :: !Double,
    boundPosition :: !Vec2
  }

type Visuals = (SV.Vector Float, SV.Vector Float)

newtype Collision = Collision Vec2

wall :: Double
wall = 10

detectCollisions :: Bound -> [Bound] -> Maybe Collision
detectCollisions a bs = case collisions a bs of
  [] -> Nothing
  c : _ -> Just c
  where
    collidesWith
      Bound {boundId = id1, boundRadius = r1, boundPosition = p1}
      Bound {boundId = id2, boundRadius = r2, boundPosition = p2} =
        id1 /= id2 && distanceSquared p1 p2 < (r1 + r2) ^ 2
    mkCollision Bound {boundPosition = p1} Bound {boundPosition = p2} = Collision (p2 .+. ((- 1) *. p1))
    collisions a = map (mkCollision a) . filter (collidesWith a)

evolveObject :: Maybe Collision -> Interval -> Object -> [Object]
evolveObject maybeCol (Interval dt) old
  | outsideWalls old = []
  | collided && objRadius old > minSplitRadius =
    [ old {objSpeed = speed', objPosition = position', objRadius = objRadius old * splitFactor},
      old {objSpeed = (- 1) *. speed', objPosition = position', objRadius = objRadius old * splitFactor}
    ]
  | otherwise = [old {objSpeed = speed', objPosition = position', objRadius = radius'}]
  where
    outsideWalls Object {objPosition = Vec2 x y} = abs x > wall || abs y > wall
    splitFactor = 0.7
    minSplitRadius = 0.1
    growthPerSec = 0.02
    maxGrowthRadius = 1
    speed' =
      case maybeCol of
        Just (Collision dir) -> reflect (objSpeed old) dir
        Nothing -> objSpeed old
    collided = isJust maybeCol
    boost = if collided then 3 else 1
    position' = objPosition old .+. ((dt * boost) *. speed')
    radius' =
      if objRadius old < maxGrowthRadius
        then objRadius old + (growthPerSec * dt)
        else objRadius old

boundObject :: Object -> Bound
boundObject Object {objId, objRadius, objPosition} =
  Bound {boundId = objId, boundRadius = objRadius, boundPosition = objPosition}

drawObject :: Object -> Visuals
drawObject Object {objRadius, objPosition, objColor} = circle objColor objRadius objPosition

data World = World
  { worldObjects :: ![Object],
    worldAge :: !Double
  }

evolveWorld :: Interval -> World -> World
evolveWorld dt world@World {worldObjects, worldAge} =
  world
    { worldObjects =
        concatMap (\a -> evolveObject (collisions a) dt a) worldObjects,
      worldAge = worldAge + interval dt
    }
  where
    bounds = fmap boundObject worldObjects
    collisions a = detectCollisions bound (walls ++ bounds)
      where
        bound@Bound {boundPosition = Vec2 x y} = boundObject a
        topWall = Bound {boundId = - 1, boundRadius = 0, boundPosition = Vec2 x wall}
        bottomWall = Bound {boundId = -2, boundRadius = 0, boundPosition = Vec2 x (- wall)}
        leftWall = Bound {boundId = -3, boundRadius = 0, boundPosition = Vec2 (- wall) y}
        rightWall = Bound {boundId = -4, boundRadius = 0, boundPosition = Vec2 wall y}
        walls = [topWall, bottomWall, leftWall, rightWall]

drawWorld :: SDL.Program -> SDL.Window -> World -> IO ()
drawWorld program window world = do
  let drawings = map drawObject (worldObjects world)
  draw program window drawings
  SDL.glSwapWindow window

simulate :: SDL.Program -> SDL.Window -> Double -> World -> IO ()
simulate program window duration initialWorld = do
  startTime <- currentTime
  loop (0, 0) 0 startTime initialWorld
  where
    fpsWindow = 0.25

    loop :: (Double, Double) -> Double -> Instant -> World -> IO ()
    loop (fpsInterval, fpsCount) total instant@(Instant i) world = do
      drawWorld program window world
      instant'@(Instant i') <- currentTime
      let dt = i' - i
          world' = evolveWorld (Interval dt) world
          total' = total + dt
      fps <-
        if fpsInterval >= fpsWindow
          then do
            let fps = fpsCount / fpsInterval
            putStrLn $ (if fps < 55 then "! " else "") ++ showDouble fps ++ "fps | " ++ show world
            pure (0, 0)
          else pure (fpsInterval + dt, fpsCount + 1)
      if total' >= duration
        then pure ()
        else loop fps total' instant' world'

initResources :: IO GL.Program
initResources = do
  -- compile shaders
  vSh <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vSh $= vsSource
  GL.compileShader vSh
  vs'Ok <- GL.get $ GL.compileStatus vSh
  unless vs'Ok $ do
    slog <- GL.get $ GL.shaderInfoLog vSh
    putStrLn $ "Vertex log:" ++ slog
    exitFailure

  -- Do it again for the fragment shader
  fSh <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS fSh $= fsSource
  GL.compileShader fSh
  fs'Ok <- GL.get $ GL.compileStatus fSh
  unless fs'Ok $ do
    slog <- GL.get $ GL.shaderInfoLog fSh
    putStrLn $ "Fragment log:" ++ slog
    exitFailure

  -- link shaders into a program
  program <- GL.createProgram
  GL.attachShader program vSh
  GL.attachShader program fSh
  GL.attribLocation program "coord" $= GL.AttribLocation 0
  GL.attribLocation program "color" $= GL.AttribLocation 1
  GL.linkProgram program
  p'Ok <- GL.get $ GL.linkStatus program
  GL.validateProgram program
  status <- GL.get $ GL.validateStatus program
  unless (p'Ok && status) $ do
    plog <- GL.get $ GL.programInfoLog program
    putStrLn plog
    exitFailure

  GL.currentProgram $= Just program

  return program

vsSource, fsSource :: BS.ByteString
vsSource =
  BS.intercalate
    "\n"
    [ "attribute vec3 coord;",
      "attribute vec3 color;",
      "varying vec3 f_color;",
      "",
      "void main(void) { ",
      " float s = 0.1f;",
      " gl_Position = vec4(coord.x * s, coord.y * s, coord.z, 1); ",
      " f_color = color;",
      "}"
    ]
fsSource =
  BS.intercalate
    "\n"
    [ "varying vec3 f_color;",
      "",
      "void main(void) { ",
      " gl_FragColor = vec4(f_color, 1);",
      "}"
    ]

circle :: Color -> Double -> Vec2 -> (SV.Vector Float, SV.Vector Float)
circle (Color r g b) rad' (Vec2 x' y') = (SV.generate coordCount vertex, SV.generate coordCount color)
  where
    rad = realToFrac rad'
    x = realToFrac x'
    y = realToFrac y'

    vertexCount = trianglesPerCircle * 3
    coordCount = vertexCount * 3
    theta = 2 * pi / fromIntegral trianglesPerCircle

    vertex i =
      let n = i `quot` 9
       in case i `rem` 9 of
            0 -> rad * cos (theta * fromIntegral n) + x
            1 -> rad * sin (theta * fromIntegral n) + y
            2 -> 0
            3 -> rad * cos (theta * fromIntegral (n + 1)) + x
            4 -> rad * sin (theta * fromIntegral (n + 1)) + y
            5 -> 0
            6 -> x
            7 -> y
            8 -> 0
            _ -> error "impossible"

    color i = case i `rem` 3 of
      0 -> r
      1 -> g
      2 -> b
      _ -> error "impossible"

draw :: GL.Program -> SDL.Window -> [(SV.Vector Float, SV.Vector Float)] -> IO ()
draw p w verticesColors = do
  let (width, height) = (fromIntegral windowWidth, fromIntegral windowHeight)
  GL.viewport $= (GL.Position 0 0, GL.Size width height)

  GL.clearColor $= GL.Color4 0.7 0.7 0.7 1
  GL.clear [GL.ColorBuffer]
  -- GL.polygonMode $= (GL.Line, GL.Line)

  GL.currentProgram $= Just p
  forM_ verticesColors $ \(vertices, colors) -> do
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    SV.unsafeWith vertices $ \ptr -> do
      GL.vertexAttribPointer (GL.AttribLocation 0)
        $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    SV.unsafeWith colors $ \ptr -> do
      GL.vertexAttribPointer (GL.AttribLocation 1)
        $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
    GL.drawArrays GL.Triangles 0 (fromIntegral (SV.length vertices) `div` 3)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled

data Color = Color !Float !Float !Float

red :: Color
red = Color 0.6 0 0

green :: Color
green = Color 0 0.6 0

blue :: Color
blue = Color 0 0 0.6

windowWidth = 900

windowHeight = 900

trianglesPerCircle = 2048

main :: IO ()
main = do
  let speedFactor = 5
      objs =
        [ Object {objId = 1, objColor = red, objRadius = 1, objPosition = Vec2 2 0, objSpeed = speedFactor *. Vec2 0 1},
          Object {objId = 2, objColor = blue, objRadius = 1, objPosition = Vec2 (-2) 0, objSpeed = speedFactor *. Vec2 (- 1) 1},
          Object {objId = 3, objColor = green, objRadius = 1, objPosition = Vec2 0 (- 1), objSpeed = speedFactor *. Vec2 1 1}
        ]
  SDL.initializeAll
  window <-
    SDL.createWindow
      (T.pack "Haskell")
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 windowWidth windowHeight,
          SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        }
  context <- SDL.glCreateContext window
  program <- initResources
  simulate program window 10 World {worldObjects = objs, worldAge = 0}
  SDL.destroyWindow window

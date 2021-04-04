{-# LANGUAGE TupleSections #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import qualified Data.Set as S

windowSize, offset :: Int
windowSize = 1000
offset = 100

fps :: Int
fps = 60

bigG :: Float
bigG = 100

-- type Point = (Float, Float)

-- | full state of the world at an instant
data World = World
  { objects :: [Object]
  , pressedKeys :: S.Set Key
  , stepSize :: Float
  } deriving (Show)

data Object = Object Point Vector Float
    deriving (Show)

-- | initial state the world starts with
initialState :: World
initialState = World
    { objects = [Object (-80,0) (0,120) 100000, Object (80,0) (0,-120) 100000]
    , pressedKeys = S.empty
    , stepSize = 0.001
    }

handleKeyEvents
  :: Event -- ^ event to handle
  -> World -- ^ initial world state
  -> World -- ^ updated world state
handleKeyEvents (EventKey (MouseButton LeftButton) Down _ (x,y)) world = world { objects = objects world ++ [Object (x,y) (0,0) 100000] }
handleKeyEvents (EventKey key Down _ _) world = world { pressedKeys = S.insert key (pressedKeys world) }
handleKeyEvents (EventKey key Up _ _) world = world { pressedKeys = S.delete key (pressedKeys world) }
handleKeyEvents _ world = world

window :: Display
window = InWindow "Gravity" (windowSize, windowSize) (offset, offset)

background :: Color
background = black

distance :: Point -> Point -> Float
distance p1 p2 = sqrt $ (fst p1 - fst p2)**2 + (snd p1 - snd p2)**2

-- | find angle between two points
angle :: Point -> Point -> Float
angle p1 p2 = atan2 (snd p2 - snd p1) (fst p2 - fst p1)

-- | find total acceleration on a point from all objects
findTotalAcc :: Point -> [Object] -> Vector
findTotalAcc pos objs = (sum $ map fst accs, sum $ map snd accs)
    where
        accs = map (findAcc pos) objs

-- | find accelearation on a point from a single object
findAcc :: Point -> Object -> Vector
findAcc pos (Object p _ m) =
    if  pos == p
        then (0,0)
        else (xAcc, yAcc)
    where
        theta = angle pos p
        dist = distance pos p
        magnitude = (bigG * m) / (dist ** 2)
        xAcc = cos theta * magnitude
        yAcc = sin theta * magnitude

-- update properties of an object
updateObj :: World -> Object -> Object
updateObj world (Object pos vel mass) = Object pos' vel' mass
    where
        step = stepSize world
        acc = findTotalAcc pos (objects world)
        pos' = (fst pos + fst vel * step, snd pos + snd vel * step)
        vel' = (fst vel + fst acc * step, snd vel + snd acc * step)

doPhysics :: World -> World
doPhysics world = world {objects = objs'}
    where
        objs = objects world
        objs' = map (updateObj world) objs

runCommands :: World -> World
runCommands world = if S.member (Char 'c') $ pressedKeys world
                       then world {objects = []}
                       else world

-- | draw lines parallel to the force at regular intervals
drawForceLines :: World -> [Picture]
drawForceLines world = map (mkLine world) positions
    where
        minPoint = - fromIntegral windowSize / 2.0 :: Float
        maxPoint = -minPoint :: Float
        positions = [(x,y) | x <- [minPoint,minPoint+20..maxPoint], y <- [minPoint,minPoint+20..maxPoint]]

-- | draw a force line at a given point
mkLine :: World -> Point -> Picture
mkLine world (x,y) = translate x y (rotate theta l)
    where
        force = findTotalAcc (x,y) (objects world)
        theta = - atan2 (snd force) (fst force) * (180/pi)
        magColor = makeColor (magV force / 1000) (1000/ magV force) 0 1
        l = color magColor $ line [(-6,0),(6,0)]

-- | take the world state and render it to a picture
render
    :: World
    -> Picture
render world = pictures $ forceLines ++ map objToPicture objs
    where
        objs = objects world
        objToPicture (Object pos _ _) = uncurry translate pos $ color blue $ circleSolid 7
        forceLines = drawForceLines world

-- update the worlds state
update :: Float -> World -> World
update _ = doPhysics . runCommands

main :: IO ()
main = play window background fps initialState render handleKeyEvents update

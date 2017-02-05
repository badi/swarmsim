import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

import qualified Numeric.LinearAlgebra as V

box = Line [(0,0), (0,100), (100,100), (100,0), (0,0)]

box_size :: Float
box_size = realToFrac norm
  where
    norm = V.norm_2 v
    v = V.fromList [100, 100] :: V.Vector Float

data Agent = Agent
             { pos :: V.Vector Float
             , vel :: V.Vector Float
             , acc :: V.Vector Float
             } deriving Show

type Model = Agent

initialModel :: Model
initialModel = Agent (V.fromList [0, 0]) (V.fromList [10, 10]) (V.fromList [100, -10])

modelToPic :: Model -> Picture
modelToPic a = p
  where
    r = pos a
    [rx, ry] = V.toList $ r
    [vx, vy] = V.toList $ r + vel a
    position = Translate rx ry $ Circle 5
    velocity = Line [(rx, ry), (vx, vy)]
    p = Pictures [box, traceShow velocity velocity, position]


nextModel :: Float -> Model -> Model
nextModel delta agent = agent'
  where
    agent' = Agent r v a
    -- r = r0 + vt + (1/2)at^2
    r = (pos agent) + (V.scale delta (vel agent)) + (V.scale (0.5*delta**2) (acc agent) )
    -- v = at + v0
    v = (vel agent) + (V.scale delta (acc agent))
    a = acc agent


step :: ViewPort -> Float -> Model -> Model
step _ t = makePeriodic . nextModel t


makePeriodic :: Agent -> Agent
makePeriodic a = a { pos = r' }
  where
    r = pos a
    r' :: V.Vector Float
    r' = V.cmap (\r -> r - fromIntegral(floor(r / 100)) * 100) r



main =
  simulate
    (InWindow "Agents" (100, 100) (0, 0))
    white
    120
    initialModel
    modelToPic
    step

-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white box

import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

import qualified Numeric.LinearAlgebra as V

box = Line [(0,0), (0,100), (100,100), (100,0), (0,0)]

data Agent = Agent
             { pos :: V.Vector Float
             , vel :: V.Vector Float
             , acc :: V.Vector Float
             } deriving Show

type Model = Agent

initialModel :: Model
initialModel = Agent (V.fromList [0, 0]) (V.fromList [10, 10]) (V.fromList [0, 0])

modelToPic :: Model -> Picture
modelToPic a = p
  where
    [x, y] = V.toList $ pos a
    [vx, vy] = V.toList $ vel a
    position = Translate x y $ Circle 5
    velocity = Translate vx vy $ Line [(x, y), (vx, vy)]
    p = Pictures [box, velocity, position]


nextModel :: Float -> Model -> Model
nextModel delta agent = traceShow (delta, agent, agent') agent'
  where
    agent' = Agent r v a
    -- r = r0 + vt + (1/2)at^2
    r = (pos agent) + (V.scale delta (vel agent)) + (V.scale (0.5*delta**2) (acc agent) )
    -- v = at + v0
    v = (vel agent) + (V.scale delta (acc agent))
    a = acc agent


step :: ViewPort -> Float -> Model -> Model
step _ = nextModel

main =
  simulate
    (InWindow "Agents" (100, 100) (0, 0))
    white
    120
    initialModel
    modelToPic
    step

-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white box

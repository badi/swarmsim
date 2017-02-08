{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SwarmSim where

import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

import qualified System.IO.Streams as Stream

-- import qualified Data.Vector
import qualified Numeric.LinearAlgebra as LA

-- http://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics
-- say = S8.putStrLn . encodeUtf8

type AgentId = Integer

newtype Tagged t a = Tagged { unTag :: a }
deriving instance Show a => Show (Tagged t a)

withTagged :: Tagged t a -> (a -> b) -> Tagged t b
withTagged t f = Tagged $! f $ unTag t

retag :: Tagged t a -> Tagged s a
retag = Tagged . unTag

-- --------------------------------------------------------------------------------

-- | Tag for mass
data M

-- | Tag for position
data X

-- | Tag for velocity
data V

-- | Tag for acceleration
data A

-- | Tag for force
data F


type Mass = Tagged M Double
type Masses = Tagged M (LA.Vector Double)

type Position = Tagged X (LA.Vector Double)
type Positions = Tagged X (LA.Matrix Double)

type Velocity = Tagged V (LA.Vector Double)
type Velocities = Tagged V (LA.Matrix Double)

type Acceleration = Tagged A (LA.Vector Double)
type Accelerations = Tagged A (LA.Matrix Double)

type Force = Tagged F (LA.Vector Double)
type Forces = Tagged F (LA.Matrix Double)

type TimeStep = Double


data State =
  MkState
  { stMass  :: !Masses
  , stPos   :: !Positions
  , stVel   :: !Velocities
  , stAcc   :: !Accelerations
  } deriving Show

data PotentialParams =
  MkPotentialParams
  { ppCuttoff :: !Double
  , ppRadius :: !Double
  , ppCharge :: !Double
  , ppOrder :: !Int
  } deriving Show

potential :: PotentialParams -> Force
potential pp = fs
  where
    fs = undefined

velocityVerlet :: TimeStep -> State -> State
velocityVerlet dt s = s'
  where
    x = unTag $! stPos s
    v = unTag $! stVel s
    a = unTag $! stAcc s
    m = unTag $! stMass s

    -- x⃗(t+Δt) = x⃗(t) + v⃗(t)Δt + 0.5a⃗(t)Δt²
    x' = x + dt`LA.scale`v + (0.5*dt^^2)`LA.scale`a

    f  = undefined
    a' = undefined

    -- v⃗(t+Δt)=v⃗(t)+0.5t(a⃗(t)+a⃗(t+Δt))Δt
    v' = v + (0.5*dt)`LA.scale`(a+a')

    s' = s
         { stPos = Tagged x'
         , stVel = Tagged v'
         , stAcc = Tagged a'
         }
  



box = Line [(0,0), (0,100), (100,100), (100,0), (0,0)]

box_size :: Float
box_size = realToFrac norm
  where
    norm = LA.norm_2 v
    v = LA.fromList [100, 100] :: LA.Vector Float

data Agent = Agent
             { pos :: LA.Vector Float
             , vel :: LA.Vector Float
             , acc :: LA.Vector Float
             } deriving Show

type Model = Agent

initialModel :: Model
initialModel = Agent (LA.fromList [0, 0]) (LA.fromList [10, 10]) (LA.fromList [100, -10])

modelToPic :: Model -> Picture
modelToPic a = p
  where
    r = pos a
    [rx, ry] = LA.toList $ r
    [vx, vy] = LA.toList $ r + vel a
    position = Translate rx ry $ Circle 5
    velocity = Line [(rx, ry), (vx, vy)]
    p = Pictures [box, traceShow velocity velocity, position]


nextModel :: Float -> Model -> Model
nextModel delta agent = agent'
  where
    agent' = Agent r v a
    -- r = r0 + vt + (1/2)at^2
    r = (pos agent) + (LA.scale delta (vel agent)) + (LA.scale (0.5*delta**2) (acc agent) )
    -- v = at + v0
    v = (vel agent) + (LA.scale delta (acc agent))
    a = acc agent


step :: ViewPort -> Float -> Model -> Model
step _ t = makePeriodic . nextModel t


makePeriodic :: Agent -> Agent
makePeriodic a = a { pos = r' }
  where
    r = pos a
    r' :: LA.Vector Float
    r' = LA.cmap (\r -> r - fromIntegral(floor(r / 100)) * 100) r



test =
  simulate
    (InWindow "Agents" (100, 100) (0, 0))
    white
    120
    initialModel
    modelToPic
    step

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SwarmSim where

import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

import qualified System.IO.Streams as Stream

import qualified Numeric.LinearAlgebra as LA

-- http://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics
-- say = S8.putStrLn . encodeUtf8



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

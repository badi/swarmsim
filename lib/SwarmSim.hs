{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SwarmSim where

import Debug.Trace

-- import Graphics.Gloss
-- import Graphics.Gloss.Data.ViewPort (ViewPort)

import qualified System.IO.Streams as Stream

-- import Data.Vector.Instances

import Linear as L
import qualified Linear.V as L
import qualified Data.Vector as V
import Control.Lens (FoldableWithIndex(..))

type Vector a = V.Vector a
type Matrix a = V.Vector (Vector a)


v,w,x :: Vector Double
v = V.fromList [2, 2]
w = 5*^v
x = V.fromList [1, 2]

m,n :: Matrix Double
m = V.fromList [v, x]
n = 2*!!m


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
type Position = Tagged X (Vector Double)
type Velocity = Tagged V (Vector Double)
type Acceleration = Tagged A (Vector Double)
type Force = Tagged F (Vector Double)
type TimeStep = Double
type Charge = Double
type Index = Int
type Distance = Double
type Neighborhood = Vector Index
type Neighbor = Position

data State =
  MkState
  { stMass  :: !(Vector Mass)
  , stCharge:: !(Vector Charge)
  , stPos   :: !(Vector Position)
  , stVel   :: !(Vector Velocity)
  , stAcc   :: !(Vector Acceleration)
  , stCutoff:: !(Vector Distance)
  , stPP    :: !PotentialParams
  } deriving Show

data PotentialParams =
  MkPotentialParams
  { ppOrder :: !Int
  , ppConstant :: !Double
  } deriving Show


distances :: Floating a => Matrix a -> Matrix a
distances m = fmap (flip distancesTo m) m


distancesTo :: Floating a => Vector a -> Matrix a -> Vector a
distancesTo v m = fmap (distance v) m


whence :: (FoldableWithIndex i f, Applicative f, Monoid (f i)) => (a -> Bool) -> f a -> f i
whence p xs = ifoldMap (\i x -> if p x then pure i else mempty) xs


neighbors :: Distance -> Position -> Vector Position -> Neighborhood
neighbors c vT mT = ns
  where
    v = unTag vT
    m = V.map unTag mT
    ns = whence (== True) $ fmap (<= c) $ distancesTo v m


-- | Find the nearest neighbors
--
-- Given an index `i`, the set of all points `ps` and their cutoff
-- radii `xs`, return the set of neighbors within `xs[i]` that are not
-- `i`.
findNeighbors :: Vector Position -> Vector Distance -> Index -> Neighborhood
findNeighbors ps xs i = ns
  where
    p = ps V.! i
    x = xs V.! i

    without :: Index -> Neighborhood -> Neighborhood
    without i' ps' = V.filter (/= i') ps'

    ns :: Neighborhood
    ns = without i $ neighbors x p ps


neighborhood :: Vector a -> Neighborhood -> Vector a
neighborhood v ixs = V.map (\i -> v V.! i) ixs
    

pp = MkPotentialParams 3 1
a :: Position
a = Tagged $ V.fromList [1,2]
q = 1.0
bs :: Vector Position
bs = V.fromList $ map Tagged [v,w]
qs :: Vector Double
qs = V.map (const 1) bs
rs :: Vector Distance
rs = V.replicate (V.length qs) 99

t1 = coulombForces pp a q bs qs
t2 = findNeighbors bs rs 0
t3 = forces pp bs rs qs

coulombForces :: PotentialParams -> Position -> Double -> Vector Position -> Vector Double -> Force
coulombForces pp x1T q1 ysT qs = Tagged f
  where
    x1 = unTag x1T
    ys = V.map unTag ysT
    effects y2 q2 = q2 *^ normalize (x1^-^y2) ^/ norm (x1^-^y2) ^ (ppOrder pp)
    summation = V.foldl1 (^+^) $ V.zipWith effects ys qs
    f = q1 / (ppConstant pp) *^ summation


forces :: PotentialParams -> Vector Position -> Vector Distance -> Vector Charge -> Vector Force
forces pp ps xs qs = V.map computeForce indices
  where
    -- [0,1,...,n]
    indices = V.generate (V.length ps) (+1)

    -- indices of the neighbors for each point
    ns :: Vector Neighborhood
    ns = V.map (findNeighbors ps xs) indices

    -- subset of coordinates, cutoffs, and chargs
    subset superset i = neighborhood superset i
    ps' :: Vector (Vector Position)
    xs' :: Vector (Vector Distance)
    qs' :: Vector (Vector Charge)
    ps' = V.map (subset ps) ns
    xs' = V.map (subset xs) ns
    qs' = V.map (subset qs) ns

    computeForce :: Index -> Force
    computeForce i = coulombForces pp (ps V.! i) (xs V.! i) (ps' V.! i) (qs' V.! i)


velocityVerlet :: TimeStep -> State -> State
velocityVerlet dt s = s'
  where
    x = V.map unTag $ stPos s
    v = V.map unTag $ stVel s
    a = V.map unTag $ stAcc s

    -- kick
    -- v(t+Δt/2) = v(t)  + a(t)Δt/2
    v_= v !+! a !!* (dt/2)
    v_ :: Matrix Double

    -- drift
    -- x(t+Δt) = x(t) + v(t)Δt + 0.5a(t)Δt²
    x' = x !+! v_ !!* dt !+! a !!* (0.5*dt^2)
    x' :: Matrix Double

    -- -- forces
    -- -- a(t+Δt) = F(x(t+Δt))
    a_ = forces (stPP s) (V.map Tagged x') (stCutoff s) (stCharge s)
    a_ :: Vector Force
    a' :: Matrix Double
    a' = V.map unTag a_

    -- kick
    -- v(t+Δt) = v(t+Δt/2)  + a(t+Δt)Δt/2
    v' = v_ !+! a' !!* (dt/2)
    v' :: Matrix Double
    
    s' = s
         { stPos = V.map Tagged x'
         , stVel = V.map Tagged v'
         , stAcc = V.map Tagged a'
         }



-- box = Line [(0,0), (0,100), (100,100), (100,0), (0,0)]

-- box_size :: Float
-- box_size = realToFrac norm
--   where
--     norm = LA.norm_2 v
--     v = LA.fromList [100, 100] :: Vector Float

-- data Agent = Agent
--              { pos :: Vector Float
--              , vel :: LA.Vector Float
--              , acc :: LA.Vector Float
--              } deriving Show

-- type Model = Agent

-- initialModel :: Model
-- initialModel = Agent (LA.fromList [0, 0]) (LA.fromList [10, 10]) (LA.fromList [100, -10])

-- modelToPic :: Model -> Picture
-- modelToPic a = p
--   where
--     r = pos a
--     [rx, ry] = LA.toList $ r
--     [vx, vy] = LA.toList $ r + vel a
--     position = Translate rx ry $ Circle 5
--     velocity = Line [(rx, ry), (vx, vy)]
--     p = Pictures [box, traceShow velocity velocity, position]


-- nextModel :: Float -> Model -> Model
-- nextModel delta agent = agent'
--   where
--     agent' = Agent r v a
--     -- r = r0 + vt + (1/2)at^2
--     r = (pos agent) + (LA.scale delta (vel agent)) + (LA.scale (0.5*delta**2) (acc agent) )
--     -- v = at + v0
--     v = (vel agent) + (LA.scale delta (acc agent))
--     a = acc agent


-- step :: ViewPort -> Float -> Model -> Model
-- step _ t = makePeriodic . nextModel t


-- makePeriodic :: Agent -> Agent
-- makePeriodic a = a { pos = r' }
--   where
--     r = pos a
--     r' :: LA.Vector Float
--     r' = LA.cmap (\r -> r - fromIntegral(floor(r / 100)) * 100) r



-- test =
--   simulate
--     (InWindow "Agents" (100, 100) (0, 0))
--     white
--     120
--     initialModel
--     modelToPic
--     step

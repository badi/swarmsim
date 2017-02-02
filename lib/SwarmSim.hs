{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module SwarmSim where

import Data.Vector (Vector)
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.Async
import Data.Word
import Control.Monad.State.Strict
import Control.Monad.Trans


newtype Coordinates = Dim2 (Int, Int) deriving Show

data Sensor = Position Coordinates deriving Show

-- | A `Message` is information related to an agent by a neighbor
data Message = Boundary Coordinates

data Event = ESensor Sensor
           | EMessage Message


data AgentState = AgentState
  { agentId :: Word8
  , agentPosition :: Coordinates
  , agentBoundaryModels :: Vector Coordinates
  , agentLogger :: forall a. Show a => a -> IO ()
  }


-- processSensor :: (MonadIO m, MonadState AgentState m) => Sensor -> m ()
processSensor = \case
  Position (Dim2 p@(x, y)) -> do
    print p


agent inChan outChan = forever $ do
  event <- readChan inChan
  case event of
    ESensor  x -> processSensor x
    EMessage x -> error "agent EMessage"


main = do
  (inChan, outChan) <- newChan
  a <- async $ agent outChan inChan

  let messages = map (ESensor . Position) $ zipWith (curry Dim2) [1..5] [5..10]
  mapM_ (writeChan inChan) messages


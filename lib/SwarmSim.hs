{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module SwarmSim where

import Data.Vector (Vector)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.Async
import Data.Word
import Control.Monad.State.Strict
import Control.Monad.Trans


data MailBox a = MkMailBox
                 { mbInChan  :: InChan  a
                 , mbOutChan :: OutChan a
                 }

newMailBox :: IO (MailBox a)
newMailBox = uncurry MkMailBox <$> newChan

readMailBox :: MailBox a -> IO a
readMailBox = readChan . mbOutChan

writeMailBox :: MailBox a -> a -> IO ()
writeMailBox box = writeChan (mbInChan box)


newtype Coordinates = Dim2 (Int, Int) deriving Show

data Sensor = Position Coordinates deriving Show

-- | A `Message` is information related to an agent by a neighbor
data Message = Boundary Coordinates

data Event = ESensor Sensor
           | EMessage Message


data AgentState = AgentState
  { agentId :: Word8
  , agentBoundaryModels :: Vector Coordinates
  , agentLogger :: forall a. Show a => a -> IO ()
  }



-- processSensor :: (MonadIO m, MonadState AgentState m) => Sensor -> m ()
processSensor = \case
  Position (Dim2 p@(x, y)) -> do
    print p


agent mailbox = forever $ do
  event <- readMailBox mailbox
  case event of
    ESensor  x -> processSensor x
    EMessage x -> error "agent EMessage"


main = do
  mailbox <- newMailBox
  runningAgent <- async $ agent mailbox

  let messages = map (ESensor . Position) $ zipWith (curry Dim2) [1..5] [5..10]
  mapM_ (writeMailBox mailbox) messages

  -- cleanup
  threadDelay (500000)
  cancel runningAgent

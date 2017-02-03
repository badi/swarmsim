{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SwarmSim where

import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO.Streams (InputStream, Generator, yield, fromGenerator)
import qualified System.IO.Streams as Stream
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.Async.Lifted
import Data.Word
import Control.Monad.State.Strict
import Control.Monad.Trans

import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

-- http://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics
say = S8.putStrLn . encodeUtf8


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
             deriving Show

data Event = ESensor Sensor
           | EMessage Message
           deriving Show


data AgentState = AgentState
  { agentId :: Word8
  , agentMailBox :: MailBox Event
  , agentPositionSensor :: InputStream Coordinates
  , agentBoundaryModels :: Vector Coordinates
  }


sensePosition :: IO (InputStream Coordinates)
sensePosition = fromGenerator $ forever $ yield (Dim2 (0,0))


readSensor :: (MonadIO m, MonadState AgentState m) => InputStream a -> m (Maybe a)
readSensor = liftIO . Stream.read


-- processSensor :: (MonadIO m, MonadState AgentState m) => Sensor -> m ()
-- processSensor sensor = do
--   case sensor of
--     Position (Dim2 p@(x, y)) -> do
--       liftIO $ print p

fireSensors :: (MonadIO m, MonadState AgentState m) => m ()
fireSensors = do
  -- liftIO $ putStrLn "Getting box"
  box <- gets agentMailBox

  -- liftIO $ say "Getting Position"
  pos <- gets agentPositionSensor >>= readSensor
  -- liftIO $ say $ pack $ show pos

  when (isJust pos) $ liftIO $ do
    -- liftIO $ say "Writing to box"
    writeMailBox box (ESensor (Position (fromJust pos)))



agent state = flip evalStateT state $ go

  where go = do

          async $ forever fireSensors
          box <- gets agentMailBox

          forever $ do
            liftIO $ say "Reading box"
            pos' <- liftIO $ readMailBox box
            liftIO $ say $ pack $ show pos'



printer :: MailBox String -> IO ()
printer box = forever $ do
  str <- readMailBox box
  putStrLn str

main = do

  -- agent 0 (Dim2 (0,0)) 0

  state <- AgentState
           <$> pure 0
           <*> newMailBox
           <*> sensePosition
           <*> pure V.empty
  runningAgent <- async $ agent state

  -- cleanup
  threadDelay (5000)
  cancel runningAgent

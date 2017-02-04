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

data AgentState = AgentState
  { agentId :: AgentId
  , agentPositionSensor :: InputStream Coordinates
  , agentBoundaryModels :: Vector Coordinates
  }


sensePosition :: IO (InputStream Coordinates)
sensePosition = fromGenerator $ forever $ yield (Dim2 (0,0))


readSensor :: (MonadIO m, MonadState AgentState m) => InputStream a -> m (Maybe a)
readSensor = liftIO . Stream.read



agentDoGetCurrentVector = error "agentDoGetCurrentVector"


agent state = flip evalStateT state $ go

  where go = do


          forever $ do
            event <- liftIO $ readMailBox box
            case event of
              Status -> do
                (pos, vel) <- agentDoGetCurrentVector
                me <- gets agentId
                
                

          forever $ do
            liftIO $ say "Reading box"
            pos' <- liftIO $ readMailBox box
            liftIO $ say $ pack $ show pos'



displayer box = do
  forever $ do
    pos <- readMailBox box
    
    undefined

main = do

  -- agent 0 (Dim2 (0,0)) 0

  state <- AgentState
           <$> pure 0
           <*> newMailBox
           <*> newMailBox
           <*> sensePosition
           <*> pure V.empty
  runningAgent <- async $ agent state

  -- cleanup
  threadDelay (5000)
  cancel runningAgent

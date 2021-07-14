module Main where

import Control.Concurrent
import Control.Monad
import Data.Functor
import Graphics.Vty
import System.Random

import Snake
import Snake.Cfg
    ( tickDelay
    , loopDelay
    , mapKey
    )

data Tick = Tick

-- | Produce tick and delay thread
tickProducer :: Int -> MVar Tick -> IO ()
tickProducer delay tick = forever $ do
    void $ tryPutMVar tick Tick
    threadDelay delay

-- | Fire tick event
tickHandler :: MVar Tick -> Game -> IO Game
tickHandler tick game
    = tryTakeMVar tick <&> maybe game (const $ dispatchEvt EvtTick game)

-- | Convert Vty events to internal events
dispatchVtyEvent :: Vty -> Game -> IO Game
dispatchVtyEvent vty game = do
    e <- nextEventNonblocking vty
    case e of
      Just (EvKey k _) -> dispatchVtyEvent vty $ dispatchEvt (mapKey k) game
      _                -> return game

-- | Main game loop
loop :: Vty -> MVar Tick -> Game -> IO ()
loop _   _    Game { state = GameQuit } = return ()
loop vty tick game                 = do
    update vty $ drawUi game
    threadDelay loopDelay
    tickHandler tick game >>= dispatchVtyEvent vty >>= loop vty tick

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    tick <- newEmptyMVar
    gen <- getStdGen
    void $ forkIO $ tickProducer tickDelay tick
    loop vty tick $ initGame gen
    shutdown vty

module Snake.Cfg where

import Graphics.Vty.Input.Events
import Snake.Types

gridW, gridH, blockW, tickDelay, loopDelay, maxDirQueue :: Int
gridW       = 15
gridH       = 15
blockW      = 2
tickDelay   = 200000
loopDelay   = 10000
-- | Maximum size of directions event queue
maxDirQueue = 5

-- | Key mapping
mapKey :: Key -> Evt
mapKey KEsc   = EvtQuit
mapKey KUp    = EvtDir North
mapKey KRight = EvtDir East
mapKey KDown  = EvtDir South
mapKey KLeft  = EvtDir West
mapKey _      = EvtOtherKey


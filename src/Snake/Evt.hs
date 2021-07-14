module Snake.Evt
    ( dispatchEvt
    ) where

import Snake.Game
import Snake.Types

-- | Event dispatcher
dispatchEvt :: Evt -> Game -> Game
dispatchEvt EvtTick game@Game { state = GamePlay }
    = step game
dispatchEvt (EvtDir dir) game
    = pushDirection dir game
dispatchEvt EvtOtherKey game@Game { state = GameOver }
    = newGame game
dispatchEvt EvtQuit game
    = game { quit = True }
dispatchEvt _ game
    = game

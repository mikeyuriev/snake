module Snake.Types
    ( Evt (..)
    , Direction (..)
    , Point (..)
    , shiftPoint
    ) where

data Evt
    = EvtTick
    | EvtQuit
    | EvtDir Direction
    | EvtOtherKey
    deriving Eq

data Direction
    = North
    | East
    | South
    | West
    deriving Eq

data Point = Point { px :: Int, py :: Int } deriving Eq

shiftPoint :: Direction -> Point -> Point
shiftPoint North p = p { py = pred $ py p }
shiftPoint East  p = p { px = succ $ px p }
shiftPoint South p = p { py = succ $ py p }
shiftPoint West  p = p { px = pred $ px p }

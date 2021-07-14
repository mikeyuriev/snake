module Snake.Game
    ( Snake
    , Food
    , GameState (..)
    , Game (..)
    , initGame
    , newGame
    , pushDirection
    , step
    ) where

import System.Random

import Snake.Cfg
    ( gridW
    , gridH
    , maxDirQueue
    )
import Snake.Types

type Snake = [Point]
type Food = Point

data GameState
    = GamePlay
    | GameOver
    | GameQuit
    deriving Eq

data Game = Game
    { score      :: Int
    , highScore  :: Int
    , state      :: GameState
    , snake      :: Snake
    , food       :: Food
    , foods      :: [Food]      -- ^ Infinity list of foods
    , direction  :: Direction
    , directions :: [Direction] -- ^ Queue of directions
    }

-- | Initialize Game
initGame :: StdGen -> Game
initGame gen = resetGame $ map toPt rnd
    where
        rnd    = randomRs (0, gridW * gridH - 1) gen
        toPt n = Point (n `mod` gridW) (n `div` gridW)

-- | Reinitialize Game
newGame :: Game -> Game
newGame Game { score = score', highScore = highScore', foods = foods' }
    = (resetGame foods') { highScore = max score' highScore' }

-- | Produce game init values
resetGame :: [Food] -> Game
resetGame foods' = Game
    { score      = 0
    , highScore  = 0
    , state      = GamePlay
    , snake      = s
    , food       = f
    , foods      = fs
    , direction  = North
    , directions = []
    }
    where
        s      = [Point (gridW `div` 2) (gridH `div` 2)] -- Center snake
        (f:fs) = dropWhile (`elem` s) foods' -- Skip not suitable points

-- | Push direction to queue
pushDirection :: Direction -> Game ->  Game
pushDirection dir game@Game { direction = direction', directions = directions' }
    | length directions' < maxDirQueue && dir /= lastDir
        = game { directions = directions' ++ [dir] }
    | otherwise
        = game
    where
        lastDir = if null directions' then direction' else last directions'

-- | Pop next direction from queue send it to step'
step :: Game -> Game
step = step' . nextDirection

-- | Shift snake (and maybe grow him), eat food, update score and game over status
step' :: Game -> Game
step' game@Game
    { score = score'
    , snake = snake'
    , food = food'
    , direction = direction'
    }
    | gameOver  = game { state = GameOver }
    | ate       = nextFood $ game { score = score' + 1, snake = p:snake' }
    | otherwise = game { snake = p:init snake' }
    where
        p@(Point x y)
            = shiftPoint direction' (head snake')
        gameOver
            = x < 0 || x >= gridW || y < 0 || y >= gridH || p `elem` snake'
        ate
            = p == food'

-- | Consume direction from queue
nextDirection :: Game -> Game
nextDirection game@Game { directions = [] }
    = game
nextDirection game@Game { directions = (d:ds) }
    = game { direction = d, directions = ds }

-- | Produce new food
nextFood :: Game -> Game
nextFood game@Game { snake = snake', foods = foods' }
    | length snake' == (gridW * gridH) = newGame game -- if can't produce food
    | otherwise                        = game { food = f, foods = fs }
    where
        (f:fs) = dropWhile (`elem` snake') foods'

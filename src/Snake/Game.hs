module Snake.Game
    ( Snake
    , Food
    , GameState (..)
    , Game (..)
    , initGame
    , newGame
    , step
    ) where

import System.Random

import Snake.Cfg
    ( gridW
    , gridH
    )
import Snake.Types

type Snake = [Point]
type Food = Point

data GameState
    = GamePlay
    | GameOver
    deriving Eq

data Game = Game
    { score     :: Int
    , quit      :: Bool
    , state     :: GameState
    , snake     :: Snake
    , food      :: Food
    , foods     :: [Food]    -- ^ Infinity list of foods
    , direction :: Direction
    }

-- | Initialize Game
initGame :: Game
initGame = resetGame $ map toPt rnd
    where
        rnd    = randomRs (0, gridW * gridH - 1) $ mkStdGen 0
        toPt n = Point (n `mod` gridW) (n `div` gridW)

-- | Reinitialize Game
newGame :: Game -> Game
newGame Game { foods = foods' }
    = resetGame foods'

-- | Produce game init values
resetGame :: [Food] -> Game
resetGame foods' = Game
    { score     = 0
    , quit      = False
    , state     = GamePlay
    , snake     = s
    , food      = f
    , foods     = fs
    , direction = North
    }
    where
        s      = [Point (gridW `div` 2) (gridH `div` 2)] -- Center snake
        (f:fs) = dropWhile (`elem` s) foods' -- Skip not suitable points

-- | Shift snake (and maybe grow him), eat food, update score and game over status
step :: Game -> Game
step game@Game
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

-- | Produce new food
nextFood :: Game -> Game
nextFood game@Game { snake = snake', foods = foods' }
    | length snake' == (gridW * gridH) = newGame game -- if can't produce food
    | otherwise                        = game { food = f, foods = fs }
    where
        (f:fs) = dropWhile (`elem` snake') foods'

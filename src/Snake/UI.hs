module Snake.UI
    ( drawUi
    ) where

import Graphics.Vty

import Snake.Cfg
    ( gridW
    , gridH
    , blockW
    )
import Snake.Game
import Snake.Types

-- | Main ui
drawUi :: Game -> Picture
drawUi game
    = Picture NoCursor [gameOver, grid, score'] (Background ' ' defAttr)
    where
        score'   = translateX (imageWidth grid + blockW) (drawScore game)
        grid     = drawGrid game
        gameOver = drawGameOver game

drawGameOver :: Game -> Image
drawGameOver Game { state = GameOver }
    = translateX (gridW * blockW `div` 2 - imageWidth txt `div` 2) -- Center text
    $ translateY (gridH `div` 2)
      txt
    where
        txt = string defAttr ("  GAME" ++ spc ++ "OVER  ")
        spc = if even gridW then " " else "  " -- For better centering
drawGameOver _
    = emptyImage

drawGrid :: Game -> Image
drawGrid Game { snake = snake', food = food' }
    = vertCat [row n | n <- [0..gridH - 1]]
    where
        row :: Int -> Image
        row y = horizCat [cell (Point x y) | x <- [0..gridW - 1]]

        cell :: Point -> Image
        cell p
            | p `elem` snake' = drawSnake
            | p == food'      = drawFood
            | otherwise       = drawEmpty

drawScore :: Game -> Image
drawScore Game { score = score' } = string defAttr ("Score: " ++ show score')

drawSnake, drawFood, drawEmpty :: Image
drawSnake = string (defAttr `withBackColor` blue) (block ' ')
drawFood  = string (defAttr `withBackColor` red) (block ' ')
drawEmpty = string (defAttr `withBackColor` brightBlack) (block ' ')

block :: Char -> String
block = replicate blockW

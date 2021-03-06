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
drawUi game = Picture
    NoCursor
    [gameOver, grid <|> translateX 2 score']
    (Background ' ' defAttr)
    where
        score', grid, gameOver :: Image
        score'   = drawScore game
        grid     = drawGrid game
        gameOver = drawGameOver game

drawGameOver :: Game -> Image
drawGameOver Game { state = GameOver }
    = translateC (gridW * blockW) gridH txt
    where
        txt :: Image
        txt = string (defAttr `withForeColor` brightWhite) ("  GAME" ++ spc ++ "OVER  ")
        spc :: String
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
            | p == head snake' = drawHead
            | p `elem` snake'  = drawSnake
            | p == food'       = drawFood
            | otherwise        = drawEmpty

drawScore :: Game -> Image
drawScore Game { score = score', highScore = highScore' } =
    string (defAttr `withForeColor` scoreFc) ("Score:      " ++ show score')
    <->
    string (defAttr `withForeColor` brightWhite) ("High Score: " ++ show highScore')
    where
        scoreFc :: Color
        scoreFc = if score' > highScore' then brightYellow else brightWhite

drawHead, drawSnake, drawFood, drawEmpty :: Image
drawHead  = string (defAttr `withBackColor` brightBlue) (block ' ')
drawSnake = string (defAttr `withBackColor` blue) (block ' ')
drawFood  = string (defAttr `withBackColor` red) (block ' ')
drawEmpty = string (defAttr `withBackColor` brightBlack) (block ' ')

block :: Char -> String
block = replicate blockW

translateCX :: Int -> Image -> Image
translateCX n img = translateX ((n - imageWidth img) `div` 2) img

translateCY :: Int -> Image -> Image
translateCY n img = translateY ((n - imageHeight img) `div` 2) img

translateC :: Int -> Int -> Image -> Image
translateC x y = translateCX x . translateCY y

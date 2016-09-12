{-
 |Game loop module implementation.

-}

module Game.Loop
    ( gameLoop
    , restartGame
    ) where

import          Control.Monad (forM_)
import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL
import          Data.Word

-- Internal modules import
import          Game.Agent
import          Game.Graphics
import          Game.State
import          Game.Text
import          Resources


statusLines :: Int
statusLines = 40


-- |
--
drawPlayBorder :: StateT GameState IO ()
drawPlayBorder = do
    gstate <- get

    let
        vw   = viewWidth gstate
        vh   = viewHeight gstate
        xl   = 160 - vw `div` 2
        yl   = (200 - statusLines - vh) `div` 2

    vwbBar (Rect 0 0 320 (200 - statusLines)) 127
    vwbBar (Rect xl yl vw vh) 0

    vwbHlin (xl - 1) (xl + vw) (yl -  1)   0
    vwbHlin (xl - 1) (xl + vw) (yl + vh) 125
    vwbVlin (yl - 1) (yl + vh) (xl -  1)   0
    vwbVlin (yl - 1) (yl + vh) (xl + vw) 125

    vwbPlot (Point (xl - 1) (yl + vw)) 124


-- |
--
drawPlayScreen :: StateT GameState IO ()
drawPlayScreen = do

    -- fadeOut

    drawPlayBorder
    -- draw the status bar

    vwbDrawPic (Point 0 (200 - statusLines)) STATUSBARPIC

    drawFace
    drawHealth
    drawLives
    drawLevel
    drawAmmo
    drawKeys
    drawWeapon
    drawScore


{- WL_GAME.C:1238 GameLoop() -}

-- |
--
restartGame :: StateT GameState IO ()
restartGame = do
    -- get current game state
    gstate <- get

    setFontColor (0, 15)
    drawPlayScreen

    put $ gstate { died      = False
                 , nextSteps = [GameLoop]
                 }


-- |
--
playLoop :: StateT GameState IO ()
playLoop = do
    gstate <- get

    let
        gdata = gameData gstate

    -- actor thinking

    return ()


-- |
--
setupGameLevel :: StateT GameState IO ()
setupGameLevel = do
    gstate <- get

    put $ gstate { timeCount        = 0
                 , secretTotal      = 0
                 , killTotal        = 0
                 , treasureTotal    = 0
                 , secretCount      = 0
                 , killCount        = 0
                 , treasureCount    = 0
                 }


-- |Main game cycle
--
gameLoop :: StateT GameState IO ()
gameLoop = do
    gstate <- get

    -- @todo
    -- if (loadedGame gstate)


    setupGameLevel

    playLoop

    put $ gstate { nextSteps = [GameLoop] }

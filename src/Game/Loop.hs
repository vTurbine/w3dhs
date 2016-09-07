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
drawPlayBorder :: GameState -> GameData -> IO ()
drawPlayBorder gstate gdata = do

    let
        vw   = viewWidth gstate
        vh   = viewHeight gstate
        xl   = 160 - vw `div` 2
        yl   = (200 - statusLines - vh) `div` 2
        surf = screen gstate

    vwb_Bar surf (Rect 0 0 320 (200 - statusLines)) 127
    vwb_Bar surf (Rect xl yl vw vh) 0

    -- vwb_Hlin surf (xl - 1) (xl + vw) (yl -  1)   0
    -- vwb_Hlin surf (xl - 1) (xl + vw) (yl + vh) 125
    -- vwb_Vlin surf (yl - 1) (yl + vh) (xl -  1)   0
    -- vwb_Vlin surf (yl - 1) (yl + vh) (xl + vw) 125

    -- vwb_Plot surf (Point (xl - 1) (yl + vw)) 124


-- |
--
drawPlayScreen :: StateT GameState IO ()
drawPlayScreen = do
    -- get current game state
    gstate <- get

    let
        gdata = gameData gstate
        pic   = (lumps gdata) !! (86 + (12 - 3)) -- STATUSBARPIC

    liftIO $ do
        fadeOut
        drawPlayBorder gstate gdata
        -- draw the status bar
        vwb_DrawPic (Point 0 (200 - statusLines)) pic

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

    let
        gdata = gameData gstate

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


-- |Main game cycle
--
gameLoop :: StateT GameState IO ()
gameLoop = do
    gstate <- get

    let
        gdata = gameData gstate

    -- @todo
    playLoop

    put $ gstate { nextSteps = [GameLoop] }

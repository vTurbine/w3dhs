{-
 |
-}

module Game.Menu
    ( us_ControlPanel
    , mainMenu
    ) where


import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL as SDL

-- Internal modules import
import          Game.State


-- |
--
us_ControlPanel :: Maybe SDLKey -> StateT GameState IO ()
us_ControlPanel key = return ()


-- |
--
mainMenu :: StateT GameState IO ()
mainMenu = do
    gstate <- get

    us_ControlPanel Nothing

    let
        nextState = if startGame gstate || loadedGame gstate
                    then RestartGame
                    else MainMenu

    put $ gstate { nextSteps = [nextState] }

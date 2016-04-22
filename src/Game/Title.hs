{-

-}

module Game.Title
    ( titleScreen
    ) where


import          Control.Monad (forM_)
import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL
import          Data.Word

-- Internal modules import
import          Game.Graphics
import          Game.State
import          Game.Text
import          Resources

--
--
titleScreen :: StateT GameState IO ()
titleScreen = do
    -- get current game state
    gstate <- get

    -- wait for input here and process to the title screens
    put $ gstate { currStep = DelayMs 3000
                 , nextStep = MainMenu
                 }
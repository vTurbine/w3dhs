{-
-}

module Game.Play
    ( playLoop
    ) where


import           Control.Monad.Trans.State

-- Internal modules import
import           Game.Draw
import           Game.State


-- |
playLoop :: StateT GameState IO ()
playLoop = do
    -- @todo many things to do before..

    threeDRefresh

    -- @todo ..and after render
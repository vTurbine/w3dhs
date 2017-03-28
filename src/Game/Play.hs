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
--
checkKeys :: StateT GameState IO ()
checkKeys = do
  -- @TODO skip when `screenfaded` or `demoplayback`
  --
  -- secret cheat code TAB-G-F10
  --
  -- secret cheat code 'MLI'
  --
  -- open-up debug keys
  -- .. and so on

  return ()


-- |
playLoop :: StateT GameState IO ()
playLoop = do
  -- @todo many things to do before..

  threeDRefresh

  checkKeys

  -- @todo ..and after render

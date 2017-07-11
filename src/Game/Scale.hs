{-
 - Scaler implementation
 -}

module Game.Scale
  ( simpleScaleShape
  ) where

import           Control.Monad.Trans.State

-- Internal modules import
import           Game.Defs
import           Game.State

-- |
--
simpleScaleShape :: Int -> SpriteNum -> Int -> StateT GameState IO ()
simpleScaleShape w s h = do
  gstate <- get
  -- ...
  return ()

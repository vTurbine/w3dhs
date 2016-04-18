module Game.State where

import Control.Monad.State
import Graphics.UI.SDL

--
--
data GameStep = IntroScreen
              | Menu
              deriving (Show)

-- |Game state record definition
--
data GameState = GameState  { step          :: GameStep
                            , printX        :: Int
                            , printY        :: Int
                            , fontColor     :: Int
                            , backColor     :: Int
                            , screen        :: Surface
                            }
                            deriving (Show)


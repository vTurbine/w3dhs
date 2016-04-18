module Game.State where

import Graphics.UI.SDL

import Resources

--
--
data GameStep = IntroScreen
              | Menu
              deriving (Show)

-- |Game state record definition
--
data GameState = GameState  { step          :: GameStep
                            , windowX       :: Int
                            , windowY       :: Int
                            , printX        :: Int
                            , printY        :: Int
                            , fontColor     :: Int
                            , backColor     :: Int
                            , screen        :: Surface
                            , gameData      :: GameData
                            }

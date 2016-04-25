module Game.State where

import Graphics.UI.SDL
import Data.Word

import Resources

--
--
data GameStep = IntroScreen
              | TitlePG13
              | TitlePage
              | MainMenu
              | WaitForInput
              | DelayMs Word32
              | Empty -- @todo really needed?
              deriving (Show)

-- |Game state record definition
--
data GameState = GameState  { currStep      :: GameStep
                            , nextStep      :: GameStep
                            , activeKeys    :: [SDLKey]           -- list of active keys
                            , windowX       :: Int
                            , windowY       :: Int
                            , printX        :: Int
                            , printY        :: Int
                            , fontColor     :: Int
                            , backColor     :: Int
                            , inputAck      :: Bool               -- is any input event occured
                            , screen        :: Surface
                            , gameData      :: GameData
                            }

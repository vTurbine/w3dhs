module Game.State where

import Graphics.UI.SDL
import Data.Word

import Resources

--
--
data GameStep = IntroBegin
              | LoadResources
              | IntroEnd
              | TitlePG13
              | TitlePage
              | Credits
              | HighScores
              | MainMenu
              | WaitForInput
              | DelayMs Word32
              | DelayMsIntr Word32 -- delay interruptible by input
              | FadeIn
              | FadeOut
              | GameLoop
              | Empty
              deriving (Show)

-- |Game state record definition
--
data GameState = GameState  { currStep      :: GameStep
                            , nextSteps     :: [GameStep]
                            , ticksPrev     :: Word32
                            , ticksCurr     :: Word32      -- ticks passed since last iteration
                            , activeKeys    :: [SDLKey]    -- list of active keys
                            , windowX       :: Int
                            , windowY       :: Int
                            , printX        :: Int
                            , printY        :: Int
                            , fontColor     :: Int
                            , backColor     :: Int
                            , inputAck      :: Bool        -- is any input event occured
                            , viewWidth     :: Int
                            , viewHeight    :: Int
                            , screen        :: Surface
                            , signon        :: [Word8]
                            , palette       :: [Color]
                            , gameData      :: GameData
                            }

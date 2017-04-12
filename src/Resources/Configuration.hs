module Resources.Configuration
    ( GameConfig(..)
    , readConfig
    , writeConfiguration
    )
    where

import Control.Monad (replicateM)
import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Data.Word

import Defs
import Settings
import SoundManager
import Utils

-- Defines from ID_US.H

-- Config file consists of sequence of data chunks
-- see WL_MAIN.C:ReadConfig() and WriteConfig() for details
--
-- Scores [0 - 6]
maxHighName     = 57
maxScores       = 7

highNameSize    = maxHighName + 1

-- Sound mode
-- Music mode
-- Digi mode
-- Mouse enabled
-- Joystick enabled
-- Joy Pad enabled
-- Joystick progressive
-- Joystick port
-- Dir scan
maxDirScan      = 4

-- Button scan
maxButtons      = 8 -- NUMBUTTONS

-- Button mouse
maxButtonMouse  = 4

-- Button joy
maxButtonJoyst  = 4

-- View size
defViewSize :: Int
defViewSize = 16

-- Mouse adjustment

data GameConfig = GameConfig { scores        :: [Score]
                             , soundMode     :: Sdmode
                             , musicMode     :: Smmode
                             , digiMode      :: Sdsmode
                             , mouseEn       :: Bool
                             , joystickEn    :: Bool
                             , joypadEn      :: Bool
                             , joystickProg  :: Bool
                             , joystickPort  :: Int
                             , dirScan       :: (Int, Int, Int, Int) -- replace
                             , buttonScan    :: [Int]
                             , buttonMouse   :: [Int]
                             , buttonJoyst   :: [Int]
                             , viewSize      :: Int
                             , mouseAdjust   :: Int
                             }
                             deriving (Show)

data Score = Score { name      :: String
                   , score     :: Int
                   , completed :: Bool
                   , episode   :: Int
                   }
                   deriving (Show)


cfgDefault :: GameConfig
cfgDefault = undefined


parseScore :: Get Score
parseScore = do
    hname <- replicateM highNameSize getWord8
    score <- getWord32le
    compl <- getWord16le
    ep    <- getWord16le
    return (Score   (bytesToString hname)
                    (fromIntegral score)
                    (toEnum . fromIntegral $ compl)
                    (fromIntegral ep))

parseConfig :: Get GameConfig
parseConfig = do
    scores <- replicateM 6 parseScore
    return (GameConfig  scores
                        Sdm_Off -- FIXME
                        Smm_Off
                        Sds_Off
                        True
                        False
                        False
                        False
                        0
                        (0, 0, 0, 0)
                        []
                        []
                        []
                        defViewSize
                        0)

-- |
--
readConfig :: BuildVariant -> IO GameConfig
readConfig v = do
  raw <- B.readFile (gameBinPath ++ "CONFIG" ++ gameBinExt v)
  let res = runGet parseConfig raw
    in case res of
      ((Right cfgUser), _) -> return cfgUser
      ((Left        _), _) -> return cfgDefault


-- |
--
writeConfiguration = undefined

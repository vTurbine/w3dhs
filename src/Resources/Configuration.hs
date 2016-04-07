module Configuration
    ( load
    , save
    )
    where

-- @todo rewrite with 'binary'

import qualified Data.ByteString.Char8 as C

import Resources
import SoundManager
import Utils

data Score = Score  { name      :: String
                    , score     :: Int
                    , completed :: Bool
                    , episode   :: Int    
                    } deriving (Show)


-- Defines from ID_US.H

-- Config file consists of sequence of data chunks
-- see WL_MAIN.C:ReadConfig() and WriteConfig() for details
--
-- Scores [0 - 6]
maxHighName     = 57
maxScores       = 7

highNameSize    = maxHighName + 1
scoreSize       = 4 -- long
completedSize   = 2 -- word
episodeSize     = 2 -- word

scoreEntrySize  = highNameSize + scoreSize + completedSize + episodeSize


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
-- Mouse adjustment

data ConfigData = ConfigData    { scores        :: (Score, Score, Score, Score, Score, Score, Score)
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
                                } deriving (Show)

getScore :: C.ByteString -> Int -> Score
getScore d n = Score (C.unpack $ C.takeWhile (/= '\NUL') highName) score completed episode
    where
        scoreData   = C.drop (n * scoreEntrySize) d
        highName    = C.take highNameSize scoreData
        d0          = C.drop highNameSize d
        score       = bytesToInt $ C.unpack (C.take scoreSize d0)
        d1          = C.drop scoreSize d0
        completed   = bytesToBool $ C.unpack (C.take completedSize d1)
        d2          = C.drop completedSize d1
        episode     = bytesToInt $ C.unpack (C.take episodeSize d2)

-- | Deserialize the data from configuration file
-- :TODO: stub
deserialize :: C.ByteString -> ConfigData
deserialize d = ConfigData (
                             getScore d 0
                           , getScore d 1
                           , getScore d 2
                           , getScore d 3
                           , getScore d 4
                           , getScore d 5
                           , getScore d 6
                           )
                            Sdm_Off
                            Smm_Off
                            Sds_Off
                            False
                            False
                            False
                            False
                            0
                            (0, 0, 0, 0)
                            []
                            []
                            []
                            0
                            0

-- | Serialize configuration structure
-- serialize :: ConfigData -> C.ByteString
-- serialize c = C.ByteString

load :: IO ConfigData
load = do
    c <- C.readFile configFile
    return (deserialize c)

save :: ConfigData -> IO ()
save = undefined

{--
configParse :: C.ByteString -> [Char]
configParse c = take (strLen s) s
    where
        s = C.unpack $ C.take 58 c
--}

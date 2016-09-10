{-|

-}

module Game.Agent
    ( drawFace
    , drawHealth
    , drawLives
    , drawLevel
    , drawAmmo
    , drawKeys
    , drawWeapon
    , drawScore
    ) where


import          Control.Monad (forM_)
import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Data.Char (digitToInt)

-- Internal modules import
import          Game.Graphics
import          Game.State
import          Resources
import          Utils


statusLines :: Int
statusLines = 40


-- |Draws a number in given location
--
latchNumber :: Point -> Int -> Int -> StateT GameState IO ()
latchNumber (Point x y) w n = do

    let
        str    = ltoa10 n
        str_ra = drop (length str) $ replicate w ' ' ++ str

        numLump n = toEnum $ n + fromEnum N_BLANKPIC

    -- draw the number
    forM_ [0..(w - 1)] (\b -> statusDrawPic (Point (x + b) y) $
                            numLump $ if str_ra !! b /= ' '
                                        then 1 + digitToInt (str_ra !! b)
                                        else 0)


-- |A wrapper around the `vwbDrawPic`
--
statusDrawPic :: Point -> GraphicNums -> StateT GameState IO ()
statusDrawPic (Point x y) = vwbDrawPic (Point (x * 8) (y + 200 - statusLines))


-- |
--
drawFace :: StateT GameState IO ()
drawFace = do
    -- get current game state
    gstate <- get

    let
        gdata = gameData gstate

    return () -- @todo


-- |Draws player's health percentage in status bar
--
drawHealth :: StateT GameState IO ()
drawHealth = do
    gstate <- get
    latchNumber (Point 21 16) 3 (health gstate)


-- |Draws player's life counter
--
drawLives :: StateT GameState IO ()
drawLives = do
    gstate <- get
    latchNumber (Point 14 16) 1 (lives gstate)


-- |
--
drawLevel :: StateT GameState IO ()
drawLevel = do
    gstate <- get
    return () -- @todo add enum for levels


-- |Draws player's ammo counter
--
drawAmmo :: StateT GameState IO ()
drawAmmo = do
    gstate <- get
    latchNumber (Point 27 16) 2 (ammo gstate)


-- |Draws the keys collected by player
--
drawKeys :: StateT GameState IO ()
drawKeys = do
    gstate <- get

    statusDrawPic (Point 30  4) $ if goldenKey $ keys gstate
                                  then GOLDKEYPIC
                                  else NOKEYPIC

    statusDrawPic (Point 30 20) $ if silverKey $ keys gstate
                                  then SILVERKEYPIC
                                  else NOKEYPIC


-- |Draws player's selected weapon
--
drawWeapon :: StateT GameState IO ()
drawWeapon = do
    gstate <- get

    let
        weapLump n = toEnum $ n + fromEnum KNIFEPIC

    statusDrawPic (Point 32 8) $ weapLump (fromEnum $ weapon gstate)


-- |Draws player's score value in status bar
--
drawScore :: StateT GameState IO ()
drawScore = do
    gstate <- get
    latchNumber (Point 6 16) 6 (score gstate)

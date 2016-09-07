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

-- |Draws a number in given location
--
latchNumber :: GameData -> Point -> Int -> Int -> IO ()
latchNumber gdata (Point x y) w n = do

    let
        str = ltoa10 n
        str_ra = drop (length str) $ replicate w ' ' ++ str

        numLump n = (lumps gdata) !! ((98 + n) + (12 - 3)) -- N_BLANKPIC

    -- draw the number
    forM_ [0..w] (\b -> statusDrawPic (Point (x + b) y) $
                    numLump $ digitToInt (str_ra !! b))


-- |
--
statusDrawPic :: Point -> Lump -> IO ()
statusDrawPic (Point x y) lump = do
    return ()


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
    gamestate <- get

    let
        gdata = gameData gamestate

    liftIO $ latchNumber gdata (Point 21 16) 3 (health gamestate)


-- |Draws player's life counter
--
drawLives :: StateT GameState IO ()
drawLives = do
    gamestate <- get

    let
        gdata = gameData gamestate

    liftIO $ latchNumber gdata (Point 14 16) 1 (lives gamestate)


-- |
--
drawLevel :: StateT GameState IO ()
drawLevel = do
    gamestate <- get

    let
        gdata = gameData gamestate
    return () -- @todo add enum for levels


-- |Draws player's ammo counter
--
drawAmmo :: StateT GameState IO ()
drawAmmo = do
    gamestate <- get

    let
        gdata = gameData gamestate

    liftIO $ latchNumber gdata (Point 27 16) 2 (ammo gamestate)


-- |Draws the keys collected by player
--
drawKeys :: StateT GameState IO ()
drawKeys = do
    gamestate <- get

    let
        gdata = gameData gamestate
        nokey = (lumps gdata) !! (95 + (12 - 3)) -- NOKEYPIC
        gkey  = (lumps gdata) !! (96 + (12 - 3)) -- GOLDKEYPIC
        skey  = (lumps gdata) !! (97 + (12 - 3)) -- SILVERKEYPIC

    liftIO $ do
        statusDrawPic (Point 30  4) $ if goldenKey $ keys gamestate -- @todo
                                      then gkey
                                      else nokey

        statusDrawPic (Point 30 20) $ if silverKey $ keys gamestate -- @todo
                                      then skey
                                      else nokey


-- |Draws player's selected weapon
--
drawWeapon :: StateT GameState IO ()
drawWeapon = do
    gamestate <- get

    let
        gdata = gameData gamestate
        weapLump n = (lumps gdata) !! ((91 + n) + (12 - 3)) -- KNIFEPIC

    liftIO $ statusDrawPic (Point 32 8) $ weapLump (fromEnum $ weapon gamestate)


-- |Draws player's score value in status bar
--
drawScore :: StateT GameState IO ()
drawScore = do
    gamestate <- get

    let
        gdata = gameData gamestate

    liftIO $ latchNumber gdata (Point 6 16) 6 (score gamestate)

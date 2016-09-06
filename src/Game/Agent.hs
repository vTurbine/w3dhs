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


import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)

-- Internal modules import
import          Game.Graphics
import          Game.State
import          Game.Text


-- |
--
{-
void StatusDrawPic (unsigned x, unsigned y, unsigned picnum)
{
    unsigned    temp;

    temp = bufferofs;
    bufferofs = 0;

    bufferofs = PAGE1START+(200-STATUSLINES)*SCREENWIDTH;
    LatchDrawPic (x,y,picnum);
    bufferofs = PAGE2START+(200-STATUSLINES)*SCREENWIDTH;
    LatchDrawPic (x,y,picnum);
    bufferofs = PAGE3START+(200-STATUSLINES)*SCREENWIDTH;
    LatchDrawPic (x,y,picnum);

    bufferofs = temp;
}
-}
statusDrawPic :: Point -> Int -> IO ()
statusDrawPic (Point x y) pic = do
    return ()


-- |
--
drawFace :: StateT GameState IO ()
drawFace = do
    -- get current game state
    gstate <- get

    let
        gdata = gameData gstate


    return ()
    {-

    if (gamestate.health)
    {
        #ifdef SPEAR
        if (godmode)
            StatusDrawPic (17,4,GODMODEFACE1PIC+gamestate.faceframe);
        else
        #endif
        StatusDrawPic (17,4,FACE1APIC+3*((100-gamestate.health)/16)+gamestate.faceframe);
    }
    else
    {
#ifndef SPEAR
     if (LastAttacker->obclass == needleobj)
       StatusDrawPic (17,4,MUTANTBJPIC);
     else
#endif
       StatusDrawPic (17,4,FACE8APIC);
    }

    -}

-- |
--
drawHealth :: StateT GameState IO ()
drawHealth = do
    return ()


-- |
--
drawLives :: StateT GameState IO ()
drawLives = do
    return ()


-- |
--
drawLevel :: StateT GameState IO ()
drawLevel = do
    return ()


-- |
--
drawAmmo :: StateT GameState IO ()
drawAmmo = do
    return ()


-- |
--
drawKeys :: StateT GameState IO ()
drawKeys = do
    return ()


-- |
--
drawWeapon :: StateT GameState IO ()
drawWeapon = do
    return ()


-- |
--
drawScore :: StateT GameState IO ()
drawScore = do
    return ()

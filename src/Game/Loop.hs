module Game.Loop
    ( gameLoop
    ) where

import          Control.Monad (forM_)
import          Control.Monad.Trans.State
import          Control.Monad.Trans (liftIO)
import          Graphics.UI.SDL
import          Data.Word

-- Internal modules import
import          Game.Graphics
import          Game.State
import          Game.Text
import          Resources

statusLines = 40


drawPlayBorder gstate gdata = do

    let
        vw = viewWidth gstate
        vh = viewHeight gstate
        xl = 160 - vw `div` 2
        yl = (200 - statusLines - vh) `div` 2
        surf = screen gstate

    vwb_Bar surf (Rect 0 0 320 (200 - statusLines)) 127

    vwb_Bar surf (Rect xl yl vw vh) 0

    --vwb_Hlin surf (xl - 1) (xl + vw) (yl -  1)   0
    --vwb_Hlin surf (xl - 1) (xl + vw) (yl + vh) 125
    --vwb_Vlin surf (yl - 1) (yl + vh) (xl -  1)   0
    --vwb_Vlin surf (yl - 1) (yl + vh) (xl + vw) 125

    --vwb_Plot surf (Point (xl - 1) (yl + vw)) 124


drawPlayerScreen :: Surface -> GameData-> IO ()
drawPlayerScreen surf gdata = do

    let
        pic = (lumps gdata) !! (86 + (12 - 3)) -- CREDITSPIC

    -- FadeOut

    -- draw the status bar
    vwb_DrawPic (Point 0 (200 - statusLines)) pic
{-

    int i,j,p,m;
    unsigned    temp;

    VW_FadeOut ();

    temp = bufferofs;

    CA_CacheGrChunk (STATUSBARPIC);

    for (i=0;i<3;i++)
    {
        bufferofs = screenloc[i];
        DrawPlayBorder ();
        VWB_DrawPic (0,200-STATUSLINES,STATUSBARPIC);
    }

    bufferofs = temp;

    UNCACHEGRCHUNK (STATUSBARPIC);

    DrawFace ();
    DrawHealth ();
    DrawLives ();
    DrawLevel ();
    DrawAmmo ();
    DrawKeys ();
    DrawWeapon ();
    DrawScore ();

-}

gameLoop :: StateT GameState IO ()
gameLoop = do
    -- get current game state
    gstate <- get

    let
        gdata = gameData gstate

    liftIO $ drawPlayBorder gstate gdata

    -- draw the intro screen
    --liftIO $ drawPlayerScreen (screen gstate) gdata

    -- wait for input here and process to the title screens
    put $ gstate { nextSteps = [GameLoop] }
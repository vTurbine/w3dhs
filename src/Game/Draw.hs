{-
-}

module Game.Draw
    ( threeDRefresh
    ) where

import           Control.Monad.Trans.State

-- Internal modules import
import           Game.State


vgaClearScreen :: StateT GameState IO ()
vgaClearScreen = undefined

wallRefresh :: StateT GameState IO ()
wallRefresh = do
    gstate <- get

    -- calculate `midangle`, `viewx` and `viewy`

    -- @TODO
    -- need to re-implement some nasty asm stuff

drawScaleds :: StateT GameState IO ()
drawScaleds = undefined

drawPlayerWeapon :: StateT GameState IO ()
drawPlayerWeapon = undefined


-- |The main render rountine
-- { WL_DRAW.C:1336 }
threeDRefresh :: StateT GameState IO ()
threeDRefresh = do
    -- 0] A strange hack with HW access. According to the comment may be skiped
    -- without any side effects

    -- 1] Clear the `spotvis` array
    -- @todo

    vgaClearScreen
    wallRefresh

    drawScaleds         -- draw scaled stuff
    drawPlayerWeapon    -- draw player's hands

    -- 2] `fizzlein` logic
    -- @todo

    -- 3] Seems that `frameon` variable isn't used. Nothing to do here

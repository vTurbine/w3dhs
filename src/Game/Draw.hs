{-
-}

module Game.Draw
    ( threeDRefresh
    ) where

import           Control.Monad.Trans.State

-- Internal modules import
import           Game.Defs
import           Game.Scale
import           Game.State

focalLength :: Int
focalLength = 22272 -- 0x5700l

vgaClearScreen :: StateT GameState IO ()
vgaClearScreen = return ()


-- |
--
asmRefresh :: StateT GameState IO ()
asmRefresh = do
  return ()


-- |
--
scalePost :: StateT GameState IO ()
scalePost = do
  return ()


-- |
--
wallRefresh :: StateT GameState IO ()
wallRefresh = do

    -- calculate `midangle`, `viewx` and `viewy`

    asmRefresh
    scalePost


-- |
--
drawScaleds :: StateT GameState IO ()
drawScaleds = do
    gstate <- get

    -- place static objects
    --
    -- place active objects
    --
    -- draw from back to front
    return ()


-- |
--
drawPlayerWeapon :: StateT GameState IO ()
drawPlayerWeapon = do
  gstate <- get

  let
    shapeNum = toEnum $ (fromEnum $
                 case (weapon gstate) of
                   Knife      -> SPR_KNIFEREADY
                   Gun        -> SPR_PISTOLREADY
                   MachineGun -> SPR_MACHINEGUNREADY
                   GatlingGun -> SPR_CHAINREADY)
               + (weaponFrame gstate)

  simpleScaleShape ((viewWidth gstate) `div` 2)
                   shapeNum
                   ((viewHeight gstate) + 1)


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

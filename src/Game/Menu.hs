{-
 |
-}

module Game.Menu
    ( mainMenu
    , mainMenuLoop
    ) where


import              Control.Monad.Trans.State
import              Control.Monad.Trans (liftIO)
-- import              Data.Word
import              Graphics.UI.SDL as SDL

-- Internal modules import
import              Game.Graphics
import              Game.State
import              Game.Text
import              Resources


type MenuRoutine = Int -> StateT GameState IO ()

data MenuItems  = NewGame
                | SoundMenu
                | Control
                | LoadGame
                | SaveGame
                | ChangeView
                | ReadThis
                | ViewScores
                | BackToDemo
                | Quit
                deriving (Enum)

-- Japan:
-- episodeSelect = [1 .. 5]
--
-- Demo:
-- episodeSelect = [1, 2]
-- newEMenu = [2, 4]
--
-- Normal:
-- episodeSelect = [1 .. 5]
-- newEMenu = [2, 4, 6, 8, 10]


-- |
--
data CpItemType = CpItemType { active   :: Bool
                             , string   :: String
                             , routine  :: StateT GameState IO ()
                             }

data CpItemInfo = CpItemInfo { pos      :: Point
                             , amount   :: Int
                             , curpos   :: Int
                             , indent   :: Int
                             }

bordColor, bord2Color, deactive, bkgdColor, stripe, textColor :: GameColor
bordColor  = 0x29
bord2Color = 0x23
deactive   = 0x2b
bkgdColor  = 0x2d
stripe     = 0x2c
textColor  = 0x17

menuX, menuY, menuW, menuH :: Int
menuX =  76
menuY =  55
menuW = 178
menuH = 13 * 10 + 6
smX   = 48
smY1  = 20
lsmX  = 85
lsmY  = 55
ctlX  = 24
ctlY  = 70
cstX  = 20
cstY  = 48
neX   = 10
neY   = 23
nmX   = 50
nmY   = 100


startItem :: Int
startItem = fromEnum NewGame


-- |
--
mainItems :: CpItemInfo
mainItems =  CpItemInfo { pos      = (Point menuX menuY)
                        , amount   = 10
                        , curpos   = startItem
                        , indent   = 24
                        }


-- |
--
sndItems :: CpItemInfo
sndItems =  CpItemInfo { pos      = (Point smX smY1)
                       , amount   = 12
                       , curpos   =  0
                       , indent   = 52
                       }


-- |
--
lsiItems :: CpItemInfo
lsiItems =  CpItemInfo { pos      = (Point lsmX lsmY)
                       , amount   = 10
                       , curpos   =  0
                       , indent   = 24
                       }


-- |
--
ctlItems :: CpItemInfo
ctlItems =  CpItemInfo { pos      = (Point ctlX ctlY)
                       , amount   =  6
                       , curpos   = -1
                       , indent   = 56
                       }


-- |
--
cusItems :: CpItemInfo
cusItems =  CpItemInfo { pos      = (Point 8 (cstY + 13 * 2))
                       , amount   = 9
                       , curpos   = -1
                       , indent   = 0
                       }


-- |
--
newEitems :: CpItemInfo
newEitems =  CpItemInfo { pos      = (Point neX neY)
                        , amount   = 11
                        , curpos   = 0
                        , indent   = 88
                        }


-- |
--
newItems :: CpItemInfo
newItems =  CpItemInfo { pos      = (Point nmX nmY)
                       , amount   = 4
                       , curpos   = 2
                       , indent   = 24
                       }


-- |
--
mainMenu_ :: [CpItemType]
mainMenu_ = [ CpItemType  { active    = True
                          , string    = "New Game"
                          , routine   = cpNewGame
                          }
            , CpItemType  { active    = True
                          , string    = "Sound"
                          , routine   = undefined -- @todo cpSound
                          }
            , CpItemType  { active    = True
                          , string    = "Control"
                          , routine   = undefined -- @todo cpControl
                          }
            , CpItemType  { active    = True
                          , string    = "Load Game"
                          , routine   = undefined -- @todo cpLoadGame
                          }
            , CpItemType  { active    = False
                          , string    = "Save Game"
                          , routine   = undefined -- @todo cpSaveGame
                          }
            , CpItemType  { active    = True
                          , string    = "Change View"
                          , routine   = undefined -- @todo cpChangeView
                          }
            ]

-- |
--
cpNewGame :: StateT GameState IO ()
cpNewGame = undefined


-- |Clears Menu screen to dark red
-- @todo SOD case
clearMScreen :: StateT GameState IO ()
clearMScreen = vwbBar (Rect 0 0 320 200) bordColor


-- |
-- @todo spear case
drawStripes :: Int -> StateT GameState IO ()
drawStripes y = do
    gstate <- get

    let
        by = if isSpear gstate
                then 24
                else 22
        w  = if isSpear gstate
                then 22
                else 23
        c  = if isSpear gstate
                then stripe
                else 0

    vwbBar  (Rect 0 y 320 by) 0
    vwbHlin 0 319 (y + w) c


-- |
--
drawOutline :: Rect -> GameColor -> GameColor -> StateT GameState IO ()
drawOutline (Rect x y w h) c1 c2 = do
    vwbHlin x (x + w)      y  c2
    vwbVlin y (y + h)      x  c2
    vwbHlin x (x + w) (y + h) c1
    vwbVlin y (y + h) (x + w) c1


-- |
--
drawWindow :: Rect -> GameColor -> StateT GameState IO ()
drawWindow r wc = do
    vwbBar r wc
    drawOutline r bord2Color deactive


-- |Sets text color (highlight or no)
-- @todo
setTextColor :: CpItemType -> Bool -> StateT GameState IO ()
setTextColor it h = return ()


-- |Handles moving gun around a menu
--
handleMenu :: CpItemInfo -> [CpItemType] -> Maybe MenuRoutine -> StateT GameState IO ()
handleMenu ii its r = do
    let
        (Point itemX itemY) = pos ii
        which = curpos ii
        x = itemX
        y = (itemY - 2) + which * 13

    vwbDrawPic (Point x y) C_CURSOR1PIC
    setTextColor (its !! which) True

    -- if redrawitem

    let
        px = itemX + indent ii
        py = itemY + which * 13

    setTextPos (Point px py)
    --usPrint (string $ its !! which)

    -- call custom routine if needed
    case r of
        Nothing  -> return ()
        (Just f) -> f which

    -- @todo


drawMenu :: CpItemInfo -> [CpItemType] -> StateT GameState IO ()
drawMenu item_i items = do
  let
    (Point x y) = pos item_i
    px          = x + (indent item_i)
    py          = y


  modify (\s -> s { windowX = px
                  , printX  = px
                  , windowY = py
                  , printY  = py
                  , windowW = 320
                  , windowH = 200
                  })

  liftIO $ print $ length items

  setFontColor (textColor, bkgdColor)

  mapM_ printItem $ zip items [0..]
    where
      (Point _ y) = pos item_i -- @todo why redefinition?
      printItem (i, n) = do
        setTextColor i (curpos item_i == n)

        gstate <- get

        if active i
          then do
            liftIO $ print $ string i
            liftIO $ print $ "X: " ++ show (printX gstate)
            liftIO $ print $ "Y: " ++ show (printY gstate)
            usPrint $ string i
          else do
            setFontColor (deactive,  bkgdColor)
            usPrint $ string i
            setFontColor (textColor, bkgdColor)

        --usPrint "\n"
        --modify (\s -> s { printY = y + n * 13 })


-- |
--
drawMainMenu :: StateT GameState IO ()
drawMainMenu = do
    clearMScreen

    vwbDrawPic (Point 112 184) C_MOUSELBACKPIC
    drawStripes 10
    vwbDrawPic (Point  84   0) C_OPTIONSPIC

    drawWindow (Rect (menuX - 8) (menuY - 3) menuW menuH) bkgdColor
    -- @todo ???
    drawMenu mainItems mainMenu_


-- |
--
mainMenuLoop :: StateT GameState IO ()
mainMenuLoop = do
    gstate <- get

    handleMenu mainItems mainMenu_ Nothing
    -- @todo

    let
        nextState = if startGame gstate || loadedGame gstate
                    then RestartGame
                    else MainMenuLoop

    put $ gstate { nextSteps = [nextState] }


-- |
--
usControlPanel :: Maybe SDLKey -> StateT GameState IO ()
usControlPanel key = do

    --cpCheckQuick
    --startCPMusic MENUSONG

    -- @todo handle scancode

    drawMainMenu
    -- menuFadeIn

    mainMenuLoop


-- |
--
mainMenu :: StateT GameState IO ()
mainMenu = do
    gstate <- get

    usControlPanel Nothing

    put $ gstate { nextSteps = [MainMenuLoop] }

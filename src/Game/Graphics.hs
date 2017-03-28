{-
 |

-}

module Game.Graphics
    ( vwbDrawPic
    , vwbBar
    , vwbVlin
    , vwbHlin
    , vwbPlot
    , vwlMeasureString
    , vwDrawPropString
    , setSurfaceData
    , Point(..)
    , updateScreen
    , vwFadeIn
    , vwFadeOut
    ) where

import              Control.Monad.Trans.State
import              Control.Monad.Trans (liftIO)
import              Control.Exception (bracket_)
import              Data.Char (ord)
import              Data.Word
import              Foreign.Marshal.Array (pokeArray)
import              Foreign.Ptr
import              Graphics.UI.SDL as SDL

-- Internal modules import
import              Game.State
import              Resources
import              Settings


data Point      = Point Int Int

paletteSize :: Int
paletteSize = 256  -- amount of elements in palette

fadeTime :: Word32
fadeTime = 20    -- delay between fade in/out steps, ms

getLumpNum :: GraphicNums -> Int
getLumpNum n = fromEnum n + (12 - 3)


-- | Copies specified lump to main surface
--
vwbDrawPic :: Point -> GraphicNums -> StateT GameState IO ()
vwbDrawPic (Point x y) n = do
  -- get current game state
  gstate <- get

  let
    gdata          = (gameData gstate)
    (Lump w h pxs) = (lumps gdata) !! getLumpNum n

  liftIO $ do
    -- create surface with size of sprite
    surf <- (createRGBSurfaceEndian [HWSurface] w h scrBpp) >>= displayFormat

    -- copy data to the surface
    setSurfaceData surf pxs

    -- blit it on the screen
    _ <- blitSurface surf Nothing (screen gstate) (Just (Rect x y w h))

    freeSurface surf


-- | A wrapper around the `fillRect`.
--
vwbBar :: Rect -> GameColor -> StateT GameState IO ()
vwbBar r c = do
  -- get current game state
  gstate <- get

  _ <- liftIO $ fillRect (screen gstate) (Just r) (Pixel $ fromIntegral c)
  return ()


-- | Draws a horizontal line with predefined width
-- TODO: handle the `linewidth` parameter
--
vlHLin :: Point -> Int -> GameColor -> StateT GameState IO ()
vlHLin (Point x y) w c = vwbBar (Rect x y w 1) c


-- | Draws a vertical line with predefined width
-- TODO: handle the `linewidth` parameter
--
vlVLin :: Point -> Int -> GameColor -> StateT GameState IO ()
vlVLin (Point x y) h c = vwbBar (Rect x y 1 h) c


-- | A wrapper around the `vlHlin`
--
vwbHlin :: Int -> Int -> Int -> GameColor -> StateT GameState IO ()
vwbHlin x1 x2 y c = vlHLin (Point x1 y) (x2 - x1 + 1) c


-- |A wrapper around the `vlVLin`
--
vwbVlin :: Int -> Int -> Int -> GameColor -> StateT GameState IO ()
vwbVlin y1 y2 x c = vlVLin (Point x y1) (y2 - y1 + 1) c


-- | Draws a point with specified color at given coordinates
--
vwbPlot :: Point -> GameColor -> StateT GameState IO ()
vwbPlot (Point x y) c = return () -- @fixme fillRect x y 1 1 seems don't work


-- | Returns size of the string in pixels
--
vwlMeasureString :: String -> StateT GameState IO (Int, Int)
vwlMeasureString str = do
  gstate <- get

  let
    gdata = gameData gstate
    font  = startFont gdata
    width = sum $ map (\c -> gWidth (font !! ord c)) str

  return (width, gHeight (font !! 0)) -- we have the height for all glyphs


colorKey :: Word8
colorKey = 0 -- used as transparence key to draw font glyphs


-- | Draws a text string at given position
--
vwDrawPropString :: Int -> Int -> String -> StateT GameState IO ()
vwDrawPropString px py s = do
  gstate <- get

  (strW, strH) <- vwlMeasureString s

  let
    gdata = gameData gstate
    f     = startFont gdata -- @todo `fontNumber`
    col   = fromIntegral $ fontColor gstate
    r     = (Rect px py strW strH)   -- otput frame
    align = 8 - (strW `mod` 8)  -- no idea why but seems that surface should be 8 pxs width aligned
    strW' = strW + align
    sdata = concatMap (\i -> (concatMap (glLine i) s) ++ padding) [0..(strH - 1)]
      where
        padding = replicate align colorKey -- padding to the alignment unit. Last character only
        glLine i c = take glW $ (drop (i * glW) $ gData (f !! (ord c)))
          where
            glW     = gWidth (f !! (ord c))

  liftIO $ do
    -- create surface with size of rect
    surf <- createRGBSurfaceEndian [HWSurface] strW' strH scrBpp >>= displayFormat

    -- set transparent color for blitting
    _ <- setColorKey surf [SrcColorKey] (Pixel 0)

    -- copy data to the surface
    -- @todo I believe it can look better
    setSurfaceData surf $ map (\p -> if p == 0 then colorKey else col) $ take (strW' * strH) sdata

    -- blit it on the screen
    _ <- blitSurface surf Nothing (screen gstate) (Just r)

    freeSurface surf


-- | Copies the list of `Word8` into given surface
--
setSurfaceData :: Surface -> [Word8] -> IO ()
setSurfaceData s ids = do
  pxs <- surfaceGetPixels s
  bracket_
    (lockSurface s)
    (unlockSurface s)
    (pokeArray (castPtr pxs) ids)


-- | Flip surfaces and update screen
--
updateScreen :: StateT GameState IO ()
updateScreen = do
  gstate <- get
  liftIO $ SDL.flip $ screen gstate


-- | Sets palette to the screen surface
--
vlSetPalette :: [Color] -> Int -> StateT GameState IO ()
vlSetPalette pal start = do
  gstate <- get

  _ <- liftIO $ SDL.setColors (screen gstate) pal start
  -- need to save recent palette since SDL doesn't have `GetPalette` function
  put $ gstate { paletteLast = pal }


-- | Returns active palette
--
vlGetPalette :: StateT GameState IO [Color]
vlGetPalette = do
  gstate <- get
  return $ paletteLast gstate


-- | Fills palette with given color
--
vlFillPalette :: Color -> StateT GameState IO ()
vlFillPalette c = do
  gstate <- get
  let
    -- WARNING: a dirty hack here. See `SDL_ConvertSurface` for details
    dummyColor = Color 1 1 1 -- need to be added in palette to prevent SDL error
    newPalette = (replicate (paletteSize - 1) c) ++ [dummyColor]

  vlSetPalette newPalette 0


-- | Fades the current palette to the given color in the
--   given number of steps
--
vlFadeOut :: Int -> Int -> Color -> Int -> StateT GameState IO ()
vlFadeOut start end c@(Color r g b) steps = do

  activePal <- vlGetPalette

  let
    numColors  = end - start + 1
    palSegment = take numColors (drop start activePal)

    changePalette :: Int -> StateT GameState IO ()
    changePalette  i | i == steps = return ()
    changePalette  i | otherwise = do
      let
        modify (Color r' g' b') = Color ((+) r' . round $ deltaR * coef)
                                        ((+) g' . round $ deltaG * coef)
                                        ((+) b' . round $ deltaB * coef)
          where
            deltaR = fromIntegral r - fromIntegral r'  -- need to cast components to prevent overflow
            deltaG = fromIntegral g - fromIntegral g'
            deltaB = fromIntegral b - fromIntegral b'
            coef   = (/) (fromIntegral i) (fromIntegral steps)

        pal' = map modify palSegment

      vlSetPalette pal' start
      liftIO $ SDL.delay fadeTime
      changePalette (i + 1)

  changePalette 0

  vlFillPalette c
  modify (\s -> s { screenFaded = True })


-- | Fades current palette to given one in the
--   given number of steps
--
vlFadeIn :: Int -> Int -> [Color] -> Int -> StateT GameState IO ()
vlFadeIn start end pal steps = do

  let
    changePalette :: Int -> StateT GameState IO ()
    changePalette i | i == steps = return ()
    changePalette i | otherwise = do

      activePal <- vlGetPalette

      let
        modify ((Color r' g' b'), idx) = Color ((+) r' . round $ deltaR * coef)
                                               ((+) g' . round $ deltaG * coef)
                                               ((+) b' . round $ deltaB * coef)
          where
            (Color r g b) = pal !! idx
            deltaR = fromIntegral r - fromIntegral r'
            deltaG = fromIntegral g - fromIntegral g'
            deltaB = fromIntegral b - fromIntegral b'
            coef   = (/) (fromIntegral i) (fromIntegral steps)

      vlSetPalette (map modify (zip activePal [0..])) 0
      liftIO $ SDL.delay fadeTime

      changePalette (i + 1)

  changePalette 0

  vlSetPalette pal 0
  modify (\s -> s { screenFaded = False })


-- | Fade screen out. Short cut
--
vwFadeOut :: StateT GameState IO ()
vwFadeOut = vlFadeOut 0 255 (Color 0 0 0) 30


-- | Fade screen in. Short cut
--
vwFadeIn :: StateT GameState IO ()
vwFadeIn = do
  gstate <- get
  vlFadeIn 0 255 (palette gstate) 30

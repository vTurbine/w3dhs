module Resources (
      GameData(..)
    , Glyph(..)
    , Lump(..)
    , loadPalette
    , loadSignon
    , loadConfig
    , loadGameData
    , module WL6
    ) where

import qualified    Data.ByteString         as B
import qualified    Data.Bitstream          as BS
import              Data.Binary.Get
import              Data.Word
import              Graphics.UI.SDL

-- Internal modules import
import              Defs
import              Resources.Configuration as Config
import              Resources.Dictionary    as Huff
import              Resources.Header        as Header
import              Resources.OMF           as OMF
import              Resources.PageFile      as PM
import              Resources.Gfxv_wl6      as WL6
import              Settings
import              Utils

type BitL = BS.Bitstream BS.Left
type BitR = BS.Bitstream BS.Right

--
--
data Lump = Lump    { w   :: Int
                    , h   :: Int
                    , pxs :: [Word8]
                    }
                    deriving (Show)


data Glyph = Glyph  { gWidth  :: Int
                    , gHeight :: Int
                    , gData   :: [Word8]
                    }
                    deriving (Show)


--
--
data GameData = GameData    { config     :: GameConfig
                            , startFont  :: [Glyph]
                            , lumps      :: [Lump]
                            }

-- | Select game resource extension in accordance to the build variant
--
gameBinExt :: BuildVariant -> String
gameBinExt BuildJapanDemo = ".WJ1"
gameBinExt BuildJapan     = ".WJ6"
gameBinExt BuildUpload    = ".WL1"
gameBinExt BuildBeta      = ".WL3"
gameBinExt BuildGoodTimes = ".WL6"
gameBinExt BuildSpearDemo = ".SDM"
gameBinExt BuildSpear     = ".SOD"


-- |The 'loadPalette' returns a palette stored into
-- GAMEPAL.OBJ file. Parsing handled by OMF loader.
--
-- The coefficient 4.0 selected as 2^(8-6) bit correction
--
loadPalette :: IO [Color]
loadPalette = do
    dat <- OMF.findResource "_gamepal" (gameSrcPath ++ "OBJ/GAMEPAL.OBJ")
    return $ wordsToColor dat
    where
        wordsToColor [] = []
        wordsToColor (r:g:b:res) = (Color (fromIntegral r * 4)
                                          (fromIntegral g * 4)
                                          (fromIntegral b * 4)) : wordsToColor res


-- |Loads signOn screen from the SIGNON.OBJ file.
-- Parsing handled by OMF loader.
--
loadSignon :: IO [Word8]
loadSignon = OMF.findResource "_signon" (gameSrcPath ++ "OBJ/SIGNON.OBJ")


-- | Load game configuration file
--
loadConfig :: BuildVariant -> IO GameConfig
loadConfig v = Config.readConfiguration (gameBinPath ++ "CONFIG" ++ gameBinExt v)


-- @todo some kinds of chunks have implicit size (see STARTTILE8M for example)
-- @todo huffman returns *more* data than expected. Now I put 'take' here to
-- truncate, but think it's better to find a reason why it happens
--
unpackChunk :: Int -> Dictionary -> [ChunkHead] -> B.ByteString -> [Word8]
unpackChunk chunkId dict heads gtData = take unp_size unp_data
    where
        (ofst, comp_size) = heads !! chunkId
        data_seg          = B.drop (fromIntegral ofst) gtData
        unp_size          = bytesToInt . B.unpack . B.take 4 $ data_seg
        comp_data         = B.take comp_size . B.drop 4 $ data_seg
        unp_data          = Huff.decode dict $ BS.unpack (BS.fromByteString comp_data :: BitL)


--
--
buildPicTable :: [Word8] -> [(Int, Int)]
buildPicTable [] = []
buildPicTable (wLo:wHi:hLo:hHi:xs) =
    (bytesToInt [wLo, wHi], bytesToInt [hLo, hHi]) : buildPicTable xs


-- @kludge rework it ASAP!
--
buildStartFont :: [Word8] -> [Glyph]
buildStartFont (hLo:hHi:xs) =
    map (\(o, w) -> Glyph w h (take (w * h) . drop (o - 770) $ d)) $ zip ofs ws
    where
        ofs = map2 bytesToInt $ take (256 * 2) xs
        ws  = map fromIntegral $ take 256 . drop (256 * 2) $ xs
        d   = drop (256 * 3) xs
        h   = bytesToInt [hLo, hHi]
        map2 _       [] = []
        map2 f (x:y:xs) = (f [x,y]) : map2 f xs


-- |Rebuilds 4x4-planes image into common pixel format
-- All credits go to Constantine Zakharchenko for his
-- brilliant idea of transformation
--
rebuildLump :: Lump -> Lump
rebuildLump (Lump w h pxs) = Lump w h (shuffle pxs)
    where
        shuffle = riffle . riffle
            where
                split pxs = splitAt ((length pxs) `div` 2) pxs

                merge (  [],   []) = []
                merge (f:fs, s:ss) = f:s:merge (fs, ss)

                riffle ar = merge (split ar)

-- | Process game resources and builds internal structures
--
loadGameData :: BuildVariant -> IO GameData
loadGameData v = do

  -- load game configuration
  config    <- loadConfig v

  -- cache game data
  grCache   <- B.readFile $ gameBinPath ++ "VGAGRAPH" ++ gameBinExt v
  hdCache   <- B.readFile $ gameBinPath ++ "VGAHEAD"  ++ gameBinExt v
  dictCache <- B.readFile $ gameBinPath ++ "VGADICT"  ++ gameBinExt v

  let
    dict      = Huff.loadDictionary dictCache
    heads     = Header.loadHeader hdCache
    pictable  = buildPicTable $ unpackChunk (fromEnum WL6.STRUCTPIC) dict heads grCache
    startfont = buildStartFont $ unpackChunk (fromEnum WL6.STARTFONT) dict heads grCache
    lumps     = map (\(n, (w, h)) -> rebuildLump $ Lump w h (unpackChunk n dict heads grCache)) $ zip [3..] pictable

  return $ GameData config
               startfont
               lumps

module Resources (
      GameData(..)
    , Glyph(..)
    , loadPalette
    , loadConfig
    , loadGameData
    ) where

import qualified    Data.ByteString         as B
import qualified    Data.Bitstream          as BS
import              Data.Binary.Get
import              Data.Word
import              Graphics.UI.SDL

-- Internal modules import
import              Resources.Configuration as Config
import              Resources.Dictionary    as Huff
import              Resources.Header        as Header
import              Resources.OMF           as OMF
import              Resources.Gfxv_wl6      as WL6
import              Settings
import              Utils

type BitL = BS.Bitstream BS.Left
type BitR = BS.Bitstream BS.Right

--
--
data Sprite = Sprite  {
                        w   :: Int
                      , h   :: Int
                      , pxs :: [Word8]
                      }


data Glyph = Glyph  { gWidth  :: Int
                    , gHeight :: Int
                    , gData   :: [Word8]
                    }
                    deriving (Show)


--
--
data GameData = GameData    { palette    :: [Color]
                            , signon     :: [Word8]
                            , config     :: GameConfig
                            , startFont  :: [Glyph]
                            , sprites    :: [Sprite]
                            }


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
loadSignOn :: IO [Word8]
loadSignOn = OMF.findResource "_signon" (gameSrcPath ++ "OBJ/SIGNON.OBJ")


--
--
loadConfig :: IO GameConfig
loadConfig = Config.readConfiguration (gameBinPath ++ "CONFIG" ++ gameBinExt)


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


-- |Processes game resources and builds internal structures
--
loadGameData :: IO GameData
loadGameData = do
    signon  <- loadSignOn
    palette <- loadPalette
    config  <- loadConfig

    grCache   <- B.readFile $ gameBinPath ++ "VGAGRAPH" ++ gameBinExt
    hdCache   <- B.readFile $ gameBinPath ++ "VGAHEAD"  ++ gameBinExt
    dictCache <- B.readFile $ gameBinPath ++ "VGADICT"  ++ gameBinExt

    let
        dict      = Huff.loadDictionary dictCache
        heads     = Header.loadHeader hdCache
        pictable  = buildPicTable $ unpackChunk (fromEnum WL6.STRUCTPIC) dict heads grCache
        startfont = buildStartFont $ unpackChunk (fromEnum WL6.STARTFONT) dict heads grCache
        sprites   = undefined -- @todo

    return $ GameData   palette
                        signon
                        config
                        startfont
                        sprites

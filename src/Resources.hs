module Resources (
      GameData(..)
    , loadPalette
    , loadSignOn
    , loadConfig
    , loadGameData
    ) where

import qualified    Data.ByteString         as B
import qualified    Data.Bitstream          as BS
import              Data.Binary.Get
import              Data.Word
import              Graphics.UI.SDL

-- Internal modules import
import              Game
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


--
--
data GameData = GameData    {
                              palette    :: [Color]
                            , signon     :: [Word8]
                            , config     :: GameConfig
                            , sprites    :: [Sprite]
                            }


-- @todo move it into separate file or load from outside
data GrChunks = STRUCTPIC
              | STARTFONT
              deriving (Show, Enum)


-- |The 'loadPalette' returns a palette stored into
-- GAMEPAL.OBJ file. Parsing handled by OMF loader.
--
loadPalette :: IO [Color]
loadPalette = do
    dat <- OMF.findResource "_gamepal" (gameSrcPath ++ "OBJ/GAMEPAL.OBJ")
    return $ wordsToColor dat
    where
        wordsToColor [] = []
        wordsToColor (r:g:b:res) = (Color (fromIntegral r) -- @todo palette adjustment (8bit)
                                          (fromIntegral g)
                                          (fromIntegral b)) : wordsToColor res


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
--
unpackChunk :: Int -> Dictionary -> [ChunkHead] -> B.ByteString -> IO [Word8]
unpackChunk chunkId dict heads gtData = do
    print $ "compressed: " ++ show comp_size ++ " @ :" ++ show ofst
    print $ B.unpack . B.take 4 $ gtData
    print $ "expected size: " ++ show unp_size
    return $ Huff.decode dict $ BS.unpack (BS.fromByteString comp_data :: BitL)
    where
        c@(ofst, comp_size) = heads !! chunkId
        data_seg          = B.drop (fromIntegral ofst) gtData
        unp_size          = bytesToInt . B.unpack . B.take 4 $ data_seg
        comp_data         = B.take comp_size . B.drop 4 $ data_seg


--
--
buildPicTable = id
--buildPicTable :: [Word8] -> [(Int, Int)]
--buildPicTable [] = []
--buildPicTable (wlo:whi:hlo:hhi:xs) =
--    (bytesToInt [wlo, whi], bytesToInt [hlo, hhi]) : buildPicTable xs



-- |Processes game resources and builds internal structures
--
loadGameData = do
    palette <- loadPalette
    signon  <- loadSignOn
    config  <- loadConfig

    grCache   <- B.readFile $ gameBinPath ++ "VGAGRAPH" ++ gameBinExt
    hdCache   <- B.readFile $ gameBinPath ++ "VGAHEAD"  ++ gameBinExt
    dictCache <- B.readFile $ gameBinPath ++ "VGADICT"  ++ gameBinExt

    let
        dict      = Huff.loadDictionary dictCache
        heads     = Header.loadHeader hdCache
        --pictable  = buildPicTable $ 
        sprites   = undefined

    a <- unpackChunk (fromEnum STRUCTPIC) dict heads grCache

    print $ "Unpacked: " ++ show (length a)

--        pt = buildPicTable $ vgaHead cache

{-
    pictable_raw <- loadGrResource $ (fromEnum STRUCTPIC) h

    putStrLn $ ":: Cached " ++ show (length pictable) ++ " chunks"
    putStrLn $ ":: Cache surface [" ++ show totalWidth ++ "x" ++ show totalHeight ++ "]"

    s <- tryCreateRGBSurfaceEndian [SWSurface] totalWidth totalHeight scrBpp
    case s of
        Nothing  -> error "Unable to create cache surface"
        (Just s) -> do
                data_raw <- loadGrResource 3
                --setSurfaceData s data_raw
                setSurfaceData scr data_raw
                return s
-}

    return $ GameData   palette
                        signon
                        config
                        sprites

module Resources
    ( loadPalette
    , loadSignOn
    , loadConfig
    , loadGrResource
    ) where

import qualified Data.ByteString as B
import qualified Data.Bitstream as BS
import Data.Binary.Get
import Data.Word
import Graphics.UI.SDL

import Resources.Configuration as Config
import Resources.Dictionary as Huff
import Resources.Header as Header
import Resources.OMF as OMF
import Settings
import Utils

type BitL = BS.Bitstream BS.Left
type BitR = BS.Bitstream BS.Right

-- |The 'loadPalette' returns a palette stored into
-- GAMEPAL.OBJ file. Parsing handled by OMF loader.
--
loadPalette :: IO [Color]
loadPalette = do
    dat <- OMF.findResource "_gamepal" (gameSrcPath ++ "OBJ/GAMEPAL.OBJ")
    return $ wordToColor dat
    where
        wordToColor [] = []
        wordToColor (r:g:b:res) = (Color (fromIntegral r) -- @todo palette adjustment (8bit)
                                         (fromIntegral g)
                                         (fromIntegral b)) : wordToColor res

-- |Loads signOn screen from the SIGNON.OBJ file.
-- Parsing handled by OMF loader.
--
loadSignOn :: IO [Word8]
loadSignOn = OMF.findResource "_signon" (gameSrcPath ++ "OBJ/SIGNON.OBJ")

loadConfig :: IO GameConfig
loadConfig = Config.readConfiguration (gameBinPath ++ "CONFIG" ++ gameBinExt)


loadGrResource :: Int -> IO [Word8]
loadGrResource c = do -- @todo Oh my God, make it lazy..
    chunk@(ofst, size) <- getChunkOffset c (gameBinPath ++ "VGAHEAD" ++ gameBinExt)
    d <- B.readFile (gameBinPath ++ "VGAGRAPH" ++ gameBinExt)
    dict <- Huff.readFile (gameBinPath ++ "VGADICT" ++ gameBinExt)

    let
        data_seg    = B.drop (fromIntegral ofst) d
        unp_size    = bytesToInt . B.unpack . B.take 4 $ data_seg
        comp_data   = B.take size . B.drop 4 $ data_seg
        unp_data    = Huff.decode dict $ BS.unpack (BS.fromByteString comp_data :: BitL)

    putStrLn $ "Loaded chunk #" ++ show c ++ " " ++ show chunk ++ " -> " ++ show unp_size
    print $ length unp_data

    return unp_data

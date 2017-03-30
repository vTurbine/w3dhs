{-
 - Map file loader
 -}

module Resources.Map
  ( MapType(..)
  , loadData
  ) where

import           Control.Monad            (replicateM)
import qualified Data.ByteString as B
import           Data.Binary.Strict.Get
import           Data.Char                (chr)
import           Data.Word

import           Utils                    (bytesToString)

numHeaders :: Int
numHeaders = 100

data MapFileType = MapFileType { rlewTag       :: Word16
                               , headerOffsets :: [Int]
                               , tileInfo      :: [Word8]
                               }
                               deriving (Show)

data MapType = MapType { planesStart :: (Int, Int, Int)
                       , panelLength :: (Int, Int, Int)
                       , mapSize     :: (Int, Int) -- width, height
                       , name        :: String
                       }
                       deriving (Show)


-- |
--
getTileInfo :: Get [Word8]
getTileInfo = do
  e <- isEmpty
  if e
    then return []
    else do
      ti   <- getWord8
      rest <- getTileInfo
      return $ ti : rest


-- |
--
parseHeader :: Get MapFileType
parseHeader = do
  e <- isEmpty
  if e
    then return $ MapFileType undefined
                         undefined
                         undefined
    else do
      tag <- getWord16le
      ofs <- replicateM numHeaders getWord32le
      tif <- getTileInfo

      return $ MapFileType tag
                      (map fromIntegral ofs)
                      tif


-- |
--
getHeader :: B.ByteString -> MapFileType
getHeader raw = do
  let res = runGet parseHeader raw
    in case res of
         ((Right mft), _) -> mft
         ((Left  err), _) -> error "Map: unable to parse header"


-- |
--
parseMapInfo :: Get MapType
parseMapInfo = do
  e <- isEmpty
  if e
     then return $
        MapType undefined
                undefined
                undefined
                undefined
      else do
        pl1 <- getWord32le
        pl2 <- getWord32le
        pl3 <- getWord32le

        plLen1 <- getWord16le -- TODO: combine into one tuple with planes?
        plLen2 <- getWord16le
        plLen3 <- getWord16le

        w <- getWord16le
        h <- getWord16le

        name <- replicateM 16 getWord8

        return $
          MapType (fromIntegral pl1, fromIntegral pl2, fromIntegral pl3)
                  (fromIntegral plLen1, fromIntegral plLen2, fromIntegral plLen3)
                  (fromIntegral w, fromIntegral h)
                  (bytesToString name)


-- |
--
getMapInfo :: B.ByteString -> MapType
getMapInfo raw = do
  let res = runGet parseMapInfo raw
    in case res of
         ((Right  mt), _) -> mt
         ((Left   err), _) -> error "Map: unable to parse gamemap"


-- |
--
loadData :: B.ByteString -> B.ByteString -> [MapType]
loadData md hdr =
  map (\i -> getMapInfo $ B.drop i md) . filter (/= 0) $ headerOffsets $ getHeader hdr

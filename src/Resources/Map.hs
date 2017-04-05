{-
 - Map file loader
 -}

module Resources.Map
  ( MapData(..)
  , loadData
  ) where

import           Control.Monad            (replicateM)
import qualified Data.ByteString as B
import           Data.Binary.Strict.Get
import           Data.Bits
import           Data.Char                (chr)
import           Data.Word

import           Utils                    (bytesToString, bsToListOfWord8
                                          ,encodeWord16le, bytesToInt)


numHeaders :: Int
numHeaders = 100

mapPlanes :: Int
mapPlanes = 2

mapWidth :: Int
mapWidth = 64

mapHeight :: Int
mapHeight = 64


data MapFileType = MapFileType { rlewTag       :: Word16
                               , headerOffsets :: [Int]
                               , tileInfo      :: [Word8]
                               }
                               deriving (Show)

data MapType = MapType { planeStart  :: (Int, Int, Int)
                       , planeLength :: (Int, Int, Int)
                       , mapWh       :: (Int, Int) -- width, height
                       , name        :: String
                       }
                       deriving (Show)

data MapData = MapData { mapSize :: (Int, Int)
                       , mapData :: ([Word8], [Word8]) -- data per planes
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
          MapType (fromIntegral pl1,    fromIntegral pl2,    fromIntegral pl3)
                  (fromIntegral plLen1, fromIntegral plLen2, fromIntegral plLen3)
                  (fromIntegral w, fromIntegral h)
                  (bytesToString name)


-- |
--
getMapInfo :: B.ByteString -> MapType
getMapInfo raw = do
  let res = runGet parseMapInfo raw
    in case res of
         ((Right   mt), _) -> mt
         ((Left   err), _) -> error "Map: unable to parse gamemap"


-- |
-- TODO
rlewExpand :: Word16 -> [Word8] -> [Word8]
rlewExpand   _         [] = []
rlewExpand tag (lo:hi:xs) = if lo == tagLo && hi == tagHi
                               then (concat $ replicate cnt val) ++ rlewExpand tag (drop 4 xs)
                               else lo : hi : rlewExpand tag xs
  where
    (tagHi, tagLo) = encodeWord16le tag
    [cntLo, cntHi] = take 2 xs
    cnt = bytesToInt [cntLo, cntHi, 0, 0]
    val = take 2 $ drop 2 xs


nearTag, farTag :: Word8
nearTag = 0xa7
farTag  = 0xa8

unp ls       [] = ls
unp ls (c:t:xs) =
  case t of
    _ | t == nearTag -> if c == 0
                           then unp (ls ++ [head xs, t]) (drop 2 xs) -- skip unsigned
                           else unp (ls ++ (concat $ replicate (fromIntegral c) dbck)) (tail xs)
      | t ==  farTag -> if c == 0
                           then unp (ls ++ [head xs, t]) (drop 2 xs)
                           else unp (ls ++ (concat $ replicate (fromIntegral c) dfwd)) (tail xs)
      | otherwise   -> unp (ls ++ [c, t]) xs
  where
    dbck   = take 2 $ drop ((length ls) - fromIntegral (head xs) * 2) ls
    dfwd   = take 2 $ drop (fromIntegral (head xs) * 2) ls


carmackExpand :: [Word8] -> [Word8]
carmackExpand [] = []
carmackExpand  d = unp [] d


-- |
-- TODO
getMapData :: B.ByteString -> MapType -> MapFileType -> ([Word8], [Word8])
getMapData md mt mft = do
  let
    tag = rlewTag mft
    (pl1ofs, pl2ofs, _) = planeStart  mt
    (pl1len, pl2len, _) = planeLength mt
    pl1comp = B.drop 2 . B.take pl1len . B.drop pl1ofs $ md -- carmacized data, drop length
    pl2comp = B.drop 2 . B.take pl2len . B.drop pl2ofs $ md
    pl1dec  = rlewExpand tag . drop 2 $ carmackExpand $ bsToListOfWord8 pl1comp
    pl2dec  = rlewExpand tag . drop 2 $ carmackExpand $ bsToListOfWord8 pl2comp

  (,) pl1dec []


-- |
--
loadData :: B.ByteString -> B.ByteString -> [MapData]
loadData md hdr = [MapData (mapWh $ mdsc !! 0) (getMapData md (mdsc !! 0) mft)]
  where
    mft  = getHeader hdr
    hdrs = filter (/= 0) $ headerOffsets mft -- skip zero offsets as unused
    mdsc = map (\i -> getMapInfo $ B.drop i md) hdrs

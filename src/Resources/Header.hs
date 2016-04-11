module Resources.Header
    ( getChunkOffset
    ) where

import Control.Monad (replicateM)
import Data.Binary.Strict.Get
import qualified Data.ByteString as B

import Utils

{-
@todo many things there change with game version (SOD/Demo/Classic..)
      but I still have no idea how to manage it. So will fix to .WL1
      data by now.
-}

getChunkOffset :: Int -> FilePath -> IO (Int, Int)
getChunkOffset c fname = do
    hdata <- Resources.Header.readFile fname
    let
        cur  = hdata !! c
        next = hdata !! (c + 1)
    return $ (cur, next - cur)


parseHeader :: Get [Int]
parseHeader = do
    empty <- isEmpty
    if empty
        then return []
        else do
            bytes <- replicateM 3 getWord8
            rest <- parseHeader

            return $ (bytesToInt $ bytes) : rest


readFile :: FilePath -> IO [Int]
readFile fname = do
    raw <- B.readFile fname
    let res = runGet parseHeader raw
        in case res of
            ((Right  ofs), _) -> return ofs
            ((Left   err), _) -> do { print $ "Header parse error: " ++ err; return [] }

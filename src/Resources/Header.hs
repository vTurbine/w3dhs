module Resources.Header (
    ChunkHead(..)
    , loadHeader
    ) where

import              Control.Monad (replicateM)
import              Data.Binary.Strict.Get
import qualified    Data.ByteString             as B

-- Internal modules import
import              Utils

-- |Chunk's offset in the data container
type Offset = Int
-- |Compressed chunk size
type Size   = Int
-- |Chunk header type
type ChunkHead = (Offset, Size)


-- |Calculates chunks sizes as differences between
-- their offsets
--
findChunks :: [Int] -> [ChunkHead]
findChunks (  _:[]) = []
findChunks (c:n:cs) = (c, n - c) : findChunks (n:cs)


-- |Header's stream parser
--
parseHeader :: Get [Int]
parseHeader = do
    empty <- isEmpty
    if empty
        then return []
        else do
            bytes <- replicateM 3 getWord8
            rest <- parseHeader

            return $ (bytesToInt $ bytes) : rest


-- |Parses input stream into list of chunk headers
-- @todo add error handling. Wrong file format (e.g. not THREEBYTEGRSTARTS)
-- may lead to crash.
--
loadHeader :: B.ByteString -> [ChunkHead]
loadHeader raw = do
    let ((Right  ofs), _) = runGet parseHeader raw
        in findChunks ofs

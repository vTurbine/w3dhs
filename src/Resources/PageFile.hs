{-
 - Page manager
 -}

module Resources.PageFile
  ( SwapData(..)
  , loadData
  ) where

import           Control.Monad            (replicateM)
import qualified Data.ByteString as B
import           Data.Binary.Strict.Get
import           Data.Word

import           Utils                    (encodeWord16le, bsToListOfWord8)


-- |Swap file header format definition
--
data FSwapHeader = FSwapHeader { chunksInFile  :: Word16
                               , pmSpriteStart :: Word16
                               , pmSoundStart  :: Word16
                               , chunksOffsets :: [Word32]
                               , chunksLengths :: [Word16]
                               }
                               deriving (Show)

-- |Loaded chunks data format
--
data SwapData = SwapData { swapChunks :: [[Word8]] }


-- |Swap file header parser
--
parseSwapHeader :: Get FSwapHeader
parseSwapHeader = do
  e <- isEmpty
  if e
     then error "Swap: the file is empty"
     else do
       chks <- getWord16le
       spst <- getWord16le
       snst <- getWord16le

       ofts <- replicateM (fromIntegral chks) getWord32le
       lens <- replicateM (fromIntegral chks) getWord16le

       return $
         FSwapHeader chks
                     spst
                     snst
                     ofts
                     lens


-- |Swap file parser runner
--
getSwapHeader :: B.ByteString -> FSwapHeader
getSwapHeader raw = do
  let res = runGet parseSwapHeader raw
    in case res of
         ((Right  pd), _) -> pd
         ((Left  err), _) -> error "Swap: unable to parse file"


-- |File loader interface implementation
--
loadData :: B.ByteString -> SwapData
loadData bs  = SwapData (map bsToListOfWord8 chks)
  where
    shdr = getSwapHeader bs
    chks = map (\(o, l) -> B.take (fromIntegral l) . B.drop (fromIntegral o) $ bs) $
               zip (chunksOffsets shdr) (chunksLengths shdr)

{-
 - Page manager
 -}

module Resources.PageFile
  ( SwapData(..)
  , CompShape(..)
  , loadData
  ) where

import           Control.Monad            (replicateM)
import qualified Data.ByteString as B
import           Data.Binary.Strict.Get
import           Data.Word

import           Utils


-- |Swap file header format definition
--
data FSwapHeader = FSwapHeader { chunksInFile  :: Int   -- total chunks in file
                               , pmSpriteStart :: Int   -- first sprite chunk #
                               , pmSoundStart  :: Int   -- first sound chunk #
                               , chunksOffsets :: [Int] -- chunk offsets
                               , chunksLengths :: [Int] -- chunk lengths
                               }
                               deriving (Show)

-- |Compiled shape data format
--
data CompShape = CompShape { leftPix   :: Int
                           , rightPix  :: Int
                           , dataOfs   :: [Int]
                           , tableData :: [Int]
                           }
                           deriving (Show)

-- |Loaded chunks data format
-- TODO: separate chunks by sprites/sounds
--
data SwapData = SwapData { spriteChunks :: [CompShape]
                         , soundChunks  :: [[Word8]]
                         }
                         deriving (Show)


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
         FSwapHeader (fromIntegral chks)
                     (fromIntegral spst)
                     (fromIntegral snst)
                     (map fromIntegral ofts)
                     (map fromIntegral lens)


-- |Swap file parser runner
--
getSwapHeader :: B.ByteString -> FSwapHeader
getSwapHeader raw = do
  let res = runGet parseSwapHeader raw
    in case res of
         ((Right  pd), _) -> pd
         ((Left  err), _) -> error "Swap: unable to parse file"


-- |Takes a sprite data from `bs` bytestream and
--  fills `CompShape` structure
--
parseSprite :: Get CompShape
parseSprite = do
  e <- isEmpty
  if e
     then error "Swap: sprite data is empty"
     else do
       lft <- getWord16le
       rgt <- getWord16le
       dof <- replicateM 64 getWord16le
       dat <- getWord8Remain

       return $
         CompShape (fromIntegral lft)
                   (fromIntegral rgt)
                   (map fromIntegral dof)
                   (map fromIntegral dat)


-- |Sprite parser runner
--
getSprite :: B.ByteString -> (Int, Int) -> CompShape
getSprite raw (o, l) = do
  let res = runGet parseSprite (B.take l $ B.drop o raw)
    in case res of
         ((Right  cs), _) -> cs
         ((Left  err), _) -> error "Swap: unable to parse sprite data"


-- |File loader interface implementation
--
loadData :: B.ByteString -> SwapData
loadData bs  = SwapData sprites
                        sounds
  where
    header      = getSwapHeader bs
    ofsSprites  = pmSpriteStart header
    ofsSounds   = pmSoundStart header
    numSprites  = ofsSounds - ofsSprites
    numSounds   = length (chunksOffsets header) - (ofsSprites + numSprites)
    sprites     = map (getSprite bs) $
                      zip (chunksOffsets header) (chunksLengths header)
    sounds      = [] -- @TODO

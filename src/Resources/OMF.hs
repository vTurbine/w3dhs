module Resources.OMF
    ( findResource
    ) where

import Control.Monad (replicateM)

import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Data.Maybe
import Data.Word

import Utils

{-

The following definitions added in acordance to the OMF v1.1 specification

 @todo
    • Add support for 16-bit and 32-bit chunks (depends on 'RecordType' value)

-}

-- | OMF file records type
--
data RecordType = THEADR           -- Translator Header
                | LHEADR           -- Library Module Header
                | COMENT           -- Comment
                | MODEND           -- Module End
                | EXTDEF           -- External Names Definition
                | PUBDEF           -- Public Names Definition
                | LINNUM
                | LNAMES
                | SEGDEF
                | GRPDEF
                | FIXUPP
                | LEDATA
                | LIDATA
                | COMDEF
                | BAKPAT
                | LEXTDEF
                | LPUBDEF
                | LCOMDEF
                | CEXTDEF
                | COMDAT
                | LINSYM
                | ALIAS
                | NBKPAT
                | LLNAMES
                | VERNUM
                | VENDEXT
                | LIBRARY_HEADER
                | LIBRARY_END
                deriving (Show, Eq)

type RawContents = [Word8]

data Record = Record { rtype    :: RecordType
                     , len      :: Int
                     , contents :: RawContents
                     , crc      :: Int
                     }
                     deriving (Show)

data PUBDEF_record = PUBDEF_record { baseGrpIdx :: Word8
                                   , baseSegIdx :: Word8
                                   , baseFrame  :: Maybe Word8
                                   , pubName    :: String
                                   , pubOffset  :: Word8
                                   }
                                   deriving (Show)

data LEDATA_record = LEDATA_record { segmentIdx   :: Word8
                                   , enumDataOffs :: Word16
                                   , dataBytes    :: [Word8]
                                   }
                                   deriving (Show)

getType td | td == 0x80 = THEADR
           | td == 0x82 = LHEADR
           | td == 0x88 = COMENT
           | td == 0x8A || td == 0x8B = MODEND
           | td == 0x8C = EXTDEF
           | td == 0x90 || td == 0x91 = PUBDEF
           | td == 0x94 || td == 0x95 = LINNUM
           | td == 0x96 = LNAMES
           | td == 0x98 || td == 0x99 = SEGDEF
           | td == 0x9A = GRPDEF
           | td == 0x9C || td == 0x9D = FIXUPP
           | td == 0xA0 || td == 0xA1 = LEDATA
           | td == 0xA2 || td == 0xA3 = LIDATA
           | td == 0xB0 = COMDEF
           | otherwise  = error "OMF parse: unexpected record type"

-- @todo add CRC verification
parseOMF :: Get [Record]
parseOMF = do
    empty <- isEmpty
    if empty
        then return []
        else do
            typeData   <- getWord8
            lengthData <- getWord16le

            let
                t = getType typeData
                l = fromIntegral lengthData - 1

            contentsData <- replicateM l getWord8
            crcData <- getWord8

            let
                d = contentsData
                c = fromIntegral crcData

            rest <- parseOMF
            return $ (Record t l d c) : rest

findRecords :: RecordType -> [Record] ->  [Record]
findRecords r = filter ((==) r . rtype)


-- |Parses PUBDEF chunk
--
parsePUBDEF :: Record -> PUBDEF_record
parsePUBDEF r = PUBDEF_record si
                              (c !! 1)
                              bf
                              (bytesToString . take len . drop (ptr + 1) $ c)
                              (c !! (ptr + len + 1))
        where
            c  = contents r
            si = (c !! 0)
            bf = if si == 0
                    then Just (c !! 2)
                    else Nothing
            -- @todo The record may contain more that one string
            -- need to handle this case recusively
            ptr = if isNothing bf -- offset to the "String Length" field
                    then 2 -- w\o "Base Frame"
                    else 3
            len = fromIntegral $ c !! ptr


-- |Parses LEDATA chunk
--
parseLEDATA :: Record -> LEDATA_record
parseLEDATA r = LEDATA_record (c !! 0)
                              0 --((c !! 1 :: Word16) + 255 * (c !! 2 :: Word16)) :: Word16
                              (drop 3 c)
        where
            c  = contents r

-- |Builds whole segment from its scattered parts.
-- @todo Sure it should be more complicated!
--
buildSegment :: [LEDATA_record] -> [Word8]
buildSegment = concatMap dataBytes


-- |Finds a resource named 's' in the OMF file 'fname'
-- and returns its data contents
--
findResource :: String -> FilePath -> IO [Word8]
findResource s fname = do
    -- read the file (@todo add multiple files too find in)
    omf <- Resources.OMF.readFile fname
    -- find the PUBDEF records with all exported names
    let
        pds   = map parsePUBDEF $ findRecords PUBDEF omf
        entry = head . filter ((==) s . pubName) $ pds -- @todo Handle the case when no entries found
        si    = baseSegIdx entry
        ds    = filter ((==) si . segmentIdx) . map parseLEDATA $ findRecords LEDATA omf
        seg   = buildSegment ds
        ofst  = fromIntegral . pubOffset $ entry

    return (drop ofst seg)


-- |The 'readFile' parses binary stream from 'fname' into
-- list of the 'Record' chunks
--
readFile :: FilePath -> IO [Record]
readFile fname = do
    raw <- B.readFile fname
    let res = runGet parseOMF raw
        in case res of
            ((Right recs), _) -> return recs
            ((Left   err), _) -> do { print $ "OMF parse error: " ++ err; return [] }

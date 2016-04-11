module Resources.Dictionary where

import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import Data.Word

type DictElem = (Word16, Word16)
data HuffTree a = Leaf a
                | Node (HuffTree a) (HuffTree a)
                --deriving (Show)

instance Show a => Show (HuffTree a) where
    show =
        go ""
        where
            spaces = map (const ' ')
            paren s = "(" ++ s ++ ")"
            go ss (Leaf s) = "--" ++ paren (show s) ++ "\n"
            go ss (Node l r) =
                let
                    root = "-+"
                    ss' = ss ++ tail (spaces root)
                    lbr = go (ss' ++ "|") l
                    rbr = go (ss' ++ " ") r
                in root ++ lbr
                        ++ ss' ++ "|\n"
                        ++ ss' ++ "`"
                        ++ rbr


parseDict :: Get [DictElem]
parseDict = do
    empty <- isEmpty
    if empty
        then return []
        else do
            left  <- getWord16le
            right <- getWord16le
            rest  <- parseDict
            return $ (left, right) : rest

buildTree :: DictElem -> [DictElem] -> HuffTree Word8
buildTree (l, r) es = Node leftT rightT
    where
        leftT  = if l < 256 -- got a value in left branch
                    then Leaf $ fromIntegral l
                    else buildTree (es !! fromIntegral (l - 256)) es
        rightT = if r < 256 -- got a value in right branch
                    then Leaf $ fromIntegral r
                    else buildTree (es !! fromIntegral (r - 256)) es

readFile fname = do
    raw <- B.readFile fname
    let
        ((Right dict), _) = runGet parseDict raw

    return $ buildTree (dict !! 254) dict


decode :: HuffTree a -> [Bool] -> [a]
decode ht = decode' ht
    where
        decode' (Node     _      _)         []   = []
        decode' (Node leftT      _) (False : bs) = decode' leftT  bs
        decode' (Node     _ rightT) (True  : bs) = decode' rightT bs
        decode' (Leaf v)                    []   = [v]
        decode' (Leaf v)                    bs   = v : decode' ht bs

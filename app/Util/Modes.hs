module Util.Modes(blockCipherEncrypt, blockCipherDecrypt, Mode(..),
                  decodeForE, encodeForE, decodeForD, encodeForD, toBlocks) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import GHC.Int(Int64)
import Data.Char

data Mode = BlockCipher
    deriving (Eq,Show)

split :: Char -> String -> [String]
split _ [] = []
split c (s:ss)
    | s == c    = []:sx
    | otherwise = (s:(head sx)):(tail sx) 
    where sx = split c ss

blockCipherEncrypt :: Integer -> (Integer -> Integer) -> String -> BL.ByteString
blockCipherEncrypt bw cipher
    = BL.concat . (map (encodeForE.cipher.decodeForE)) . toBlocks (fromInteger bw) . BLU.fromString

blockCipherDecrypt :: Integer -> (Integer -> Integer) -> BL.ByteString -> String
blockCipherDecrypt bw decipher
    = BLU.toString . BL.concat . map (encodeForD bw.decipher.decodeForD) . split '/' . BLU.toString


toBlocks :: Int64 -> BL.ByteString -> [BL.ByteString]
toBlocks bw bs
    | BL.null bs = []
    | otherwise  = (BL.take bw bs):(toBlocks bw $ BL.drop bw bs)

decodeForE :: BL.ByteString -> Integer
decodeForE = BL.foldr (\x y -> (toInteger x) + (256 * y)) 0

encodeForE :: Integer -> BL.ByteString
encodeForE i = BLU.fromString (repInt i ++ "/")

decodeForD :: String -> Integer
decodeForD = decodeForD'
decodeForD' :: String -> Integer
decodeForD' []     = 0
decodeForD' (s:ss) = (toInteger $ ord s - 48) + 64 * (decodeForD' ss)

encodeForD :: Integer -> Integer -> BL.ByteString
encodeForD bw val = BL.append unpadded padding
    where unpadded = BL.unfoldr (\x -> if x == 0 then Nothing
                                       else Just (fromIntegral (x `mod` 256), x `div` 256)) val
          padding = BL.replicate (fromIntegral bw - BL.length unpadded) 0

repInt :: Integer -> String
repInt 0 = ""
repInt i = (chr . fromIntegral $ (m + 48)) : (repInt i')
    where (i',m) = i `divMod` 64



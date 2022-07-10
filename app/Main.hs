module Main where

import System.Environment
import System.Exit
import Util.RSA as RSA
import Util.Modes
import Text.Read
import System.Random as Rand
import Control.Monad
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

rsaKeyLenMax = 2100
rsaKeyLenMin = 256

extPubKey  = ".lcpub"
extPrivKey = ".lcpriv"
extRSAEncryptedFile = ".lcrk"

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

isDigit c  = c `elem` "0123456789"
isLetter c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

data Action = GenRSAKeyPair (Int, String) | Help | Invalid |
              RSAEncrypt (String,String,String,Mode) |
              RSADecrypt (String,String,String,Mode)
    deriving (Eq,Show)

doAction Invalid = do putStrLn "Invalid command"
                      return $ exitWith ExitSuccess

doAction Help = do putStrLn "No tutorials just now"
                   return $ exitWith ExitSuccess
                          
doAction (GenRSAKeyPair (keylen, filePath))
    = do rand <- return Rand.initStdGen
         kp   <- RSA.generateKeyPair rand (2^a,2^(a+1)) (2^b,2^(b+1))
         writeKeyPair filePath kp         
    where a = toInteger (keylen `div` 2 - 2)
          b = toInteger (keylen `div` 2 + 2)

doAction (RSAEncrypt (keypath, input, output, BlockCipher))
    = do key       <- readPubKey keypath
         inHandle  <- openFile input ReadMode
         outHandle <- openFile (output ++ extRSAEncryptedFile) WriteMode
         n         <- return $ (\(Just (RSA.PublicKey (e,n))) -> n) key
         inData    <- hGetContents inHandle
         encryptedBS <- return $ blockCipherEncrypt (width n) ((\(Just x) -> encrypt x) key) inData
         BL.hPut outHandle encryptedBS
         putStrLn (BLU.toString encryptedBS)
         putStrLn "\n encryption successful"
         hClose inHandle
         hClose outHandle
         return $ exitWith ExitSuccess
      where width x = if x == 0 then -3 else 1 + (width $ x `div` 256) 

doAction (RSADecrypt (keypath, input, output, BlockCipher))
    = do key       <- readPrivKey keypath
         inHandle  <- openFile (input ++ extRSAEncryptedFile) ReadMode
         outHandle <- openFile output WriteMode
         n         <- return $ (\(Just (RSA.PrivateKey (d,n))) -> n) key
         inData    <- BL.hGetContents inHandle
         decryptedBS <- return $ blockCipherDecrypt (width n) ((\(Just x) -> decrypt x) key) inData
         hPutStr outHandle decryptedBS
         putStrLn decryptedBS
         putStrLn "decryption successful"
         hClose inHandle
         hClose outHandle
         return $ exitWith ExitSuccess
      where width x = if x == 0 then -3 else 1 + (width $ x `div` 256) 
         
writeKeyPair _ Nothing = do return $ putStrLn "Failed to generate key-pair"
                            return $ exitWith ExitSuccess
writeKeyPair path (Just (RSA.PublicKey (e,n), RSA.PrivateKey (d,_)))
    = do pubhandle <- openFile (path ++ extPubKey) WriteMode
         prihandle <- openFile (path ++ extPrivKey) WriteMode
         hPutStr pubhandle ("e=" ++ (show e) ++ ".\nn=" ++ (show n) ++ ".")
         hPutStr prihandle ("d=" ++ (show d) ++ ".\nn=" ++ (show n) ++ ".")
         hClose pubhandle
         hClose prihandle
         putStrLn "Successfully saved keys"
         return $ exitWith ExitSuccess

readPrivKey :: String -> IO (Maybe RSA.PrivateKey)
readPrivKey path
    = do handl    <- catchAny (openFile (path ++ extPrivKey) ReadMode)
                              (\_ -> do putStrLn ("Cannot open " ++ path ++ extPrivKey)
                                        exitWith ExitSuccess)
         contents <- hGetContents handl
         nums     <- return $ readNumbersFromStr contents
         d        <- return $ findBy 'd' nums
         n        <- return $ findBy 'n' nums
         return $ case constrPair d n of
                       Nothing -> Nothing
                       Just p  -> Just (RSA.PrivateKey p)

readPubKey :: String -> IO (Maybe RSA.PublicKey)
readPubKey path
    = do handl    <- catchAny (openFile (path ++ extPubKey) ReadMode)
                              (\_ -> do putStrLn ("Cannot open " ++ path ++ extPrivKey)
                                        exitWith ExitSuccess)
         contents <- hGetContents handl
         nums     <- return $ readNumbersFromStr contents
         e        <- return $ findBy 'e' nums
         n        <- return $ findBy 'n' nums
         return $ case constrPair e n of
                       Nothing -> Nothing
                       Just p  -> Just (RSA.PublicKey p)

constrPair :: Maybe a -> Maybe b -> Maybe (a,b)
constrPair x y
    = case x of
           Nothing -> Nothing
           Just x' -> case y of
                           Nothing -> Nothing
                           Just y' -> Just (x',y')

findBy :: (Eq a) => a -> [(a,b)] -> Maybe b
findBy z ((x,y):xs)
    | x == z    = Just y
    | otherwise = findBy z xs
findBy _ [] = Nothing

readNumbersFromStr :: String -> [(Char,Integer)]
readNumbersFromStr (x:y:xs)
    | isLetter x && y == '=' = (x, read $ takeWhile isDigit xs) : (readNumbersFromStr $ dropWhile isDigit xs)
    | otherwise = readNumbersFromStr (y:xs)
readNumbersFromStr _ = []

parseArgs :: [String] -> IO Action
parseArgs (x:xs)
    | x == "rsagen" = return $ case (parseGenRSAKeyPairArgs xs) of
                                    Nothing -> Invalid
                                    Just x  -> GenRSAKeyPair x
    | x == "rsaenc" = return $ case (parseRSAEncDecArgs xs) of
                                    Nothing -> Invalid
                                    Just x  -> RSAEncrypt x
    | x == "rsadec" = return $ case (parseRSAEncDecArgs xs) of
                                    Nothing -> Invalid
                                    Just x  -> RSADecrypt x
    | otherwise     = return Invalid
parseArgs [] = return Invalid

parseGenRSAKeyPairArgs :: [String] -> Maybe (Int, String)
parseGenRSAKeyPairArgs (x:y:xs)
    = case readMaybe x of
           Nothing -> Nothing
           Just v -> if v < rsaKeyLenMin || v > rsaKeyLenMax
                     then Nothing
                     else Just (v,y)
parseGenRSAKeyPairArgs _ = Nothing

parseRSAEncDecArgs :: [String] -> Maybe (String,String,String,Mode)
parseRSAEncDecArgs (k:i:o:m:xs)
    | m == "-b" = Just (k,i,o,BlockCipher)
    | otherwise = Nothing
parseRSAEncDecArgs _ = Nothing

main = do args   <- getArgs
          action <- parseArgs args
          doAction action
          exitWith ExitSuccess
          
          

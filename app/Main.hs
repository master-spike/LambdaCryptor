module Main where

import System.Environment
import System.Exit
import Util.RSA as RSA
import Text.Read
import System.Random as Rand
import Control.Monad
import System.IO
import Control.Exception

rsaKeyLenMax = 2100
rsaKeyLenMin = 256

extPubKey  = ".lcpub"
extPrivKey = ".lcpriv" 

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch


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



data Action = GenRSAKeyPair (Int, String) | Help
    deriving Show

doAction Nothing     = do putStrLn "Invalid command"
                          return $ exitWith ExitSuccess

doAction (Just Help) = do putStrLn "No tutorials just now"
                          return $ exitWith ExitSuccess
                          
doAction (Just (GenRSAKeyPair (keylen, filePath)))
    = do rand <- return Rand.initStdGen
         kp   <- RSA.generateKeyPair rand (2^a,2^(a+1)) (2^b,2^(b+1))
         writeKeyPair filePath kp
         
    where a = toInteger (keylen `div` 2 - 2)
          b = toInteger (keylen `div` 2 + 2)

parseArgs :: [String] -> IO (Maybe Action)
parseArgs (x:xs)
    | x == "rsagen" = return (parseGenRSAKeyPairArgs xs >>= return . GenRSAKeyPair)
    | otherwise     = return Nothing
parseArgs [] = return Nothing

parseGenRSAKeyPairArgs :: [String] -> Maybe (Int, String)
parseGenRSAKeyPairArgs (x:y:xs)
    = case readMaybe x of
           Nothing -> Nothing
           Just v -> if v < rsaKeyLenMin || v > rsaKeyLenMax
                     then Nothing
                     else Just (v,y)
parseGenRSAKeyPairArgs _ = Nothing

main = do args   <- getArgs
          action <- parseArgs args
          doAction action
          exitWith ExitSuccess
          
          

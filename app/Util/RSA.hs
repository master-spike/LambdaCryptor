module Util.RSA(generateKeyPair, PublicKey(..), PrivateKey(..), encrypt, decrypt) where

import Util.CryptoMath
import System.Random

defaultE :: Integer
defaultE = 65537

newtype PublicKey = PublicKey (Integer,Integer)
    deriving (Eq,Show)
newtype PrivateKey = PrivateKey (Integer,Integer)
    deriving (Eq,Show)

createKeyPair :: Integer -> Integer -> Integer -> Maybe (PublicKey,PrivateKey)
createKeyPair e p q
    | not (baillePSWTest p) || not (baillePSWTest q)    = Nothing
    | gcd e l /= 1 || e <= 1 || e >= l || gcd e n /= 1  = Nothing
    | otherwise = Just (PublicKey (e,n) , PrivateKey (d `mod` l,n))
    where l = lcm (p-1) (q-1)
          n = p * q
          ((d,_),_,_) = extendedEuclideanGCD e l

generateKeyPair :: (Monad m) => m StdGen -> (Integer,Integer) -> (Integer,Integer) -> m (Maybe (PublicKey,PrivateKey))
generateKeyPair g1 ip iq = g1 >>= return . randomR ip
                              >>= (\(x,g2) -> return (x, fst $ randomR iq g2))
                              >>= (\(x,y)  -> return (getNextBPSW x, getNextBPSW y))
                              >>= (\(x,y)  -> return $ createKeyPair defaultE x y)
                              

encrypt :: Integer -> PublicKey -> Integer
decrypt :: Integer -> PrivateKey -> Integer

encrypt m (PublicKey (e,n))  = modPow n m e
decrypt c (PrivateKey (d,n)) = modPow n c d



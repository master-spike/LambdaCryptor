module Util.CryptoMath(modAdd, modTimes, modPow, getNextPrime, isPrime, getNextBPSW, baillePSWTest,
                       fermatPTest, floorSqrt, millerRabinTest, lucasPrimeTest) where
                       
import Data.List

modAdd   :: Integer -> Integer -> Integer -> Integer
modTimes :: Integer -> Integer -> Integer -> Integer
modPow   :: Integer -> Integer -> Integer -> Integer


modAdd m a b = if y >= 0 then y else y + m
    where y = (a + b) `mod` m
    
modTimes m a b = if y >= 0 then y else y + m
    where y = (a * b) `mod` m

modPow m a b
    | b == 0 = 1
    | even b = modPow  m (modTimes m a a) (b `div` 2)
    | otherwise = modTimes m a $ modPow m a (b-1)
    
    
getNextPrime :: Integer -> Integer
getNextPrime n = if isPrime (n+1) then n+1 else getNextPrime (n+1)

getNextBPSW :: Integer -> Integer
getNextBPSW n = head . filter baillePSWTest $ [n+1..]

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
    | n < 2                 = False
    | even n                = False
    | cs smallPrimes        = False
    | not (fermatPTest 2 n) = False
    | isPrime' n            = True
    | otherwise             = False
    where cs []     = False
          cs (x:xs)
              | n == x         = False
              | n `mod` x == 0 = True
              | otherwise      = cs xs
              
isPrime' :: Integer -> Bool
isPrime' n = checkall . (filter $ fermatPTest 2) $ [1001,1003..(floorSqrt n)]
   where checkall [] = True
         checkall (x:xs)
             | n == x         = True
             | n `mod` x == 0 = False
             | otherwise      = checkall xs
             
             
fermatPTest :: Integer -> Integer -> Bool
fermatPTest a n = modPow n a (n-1) == 1

floorSqrt :: Integer -> Integer
floorSqrt n = bsSqrt n 0 n
bsSqrt :: Integer -> Integer -> Integer -> Integer
bsSqrt n a b
    | (b-a) <= 1 = a
    | m*m > n    = bsSqrt n a m
    | otherwise  = bsSqrt n m b
    where m = (a+b) `div` 2

smallPrimes :: [Integer]
smallPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,
               113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,
               251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,
               397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,
               557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,
               701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,
               863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997]
               
millerRabinTest :: Integer -> Integer -> Bool
millerRabinTest a n = True `elem` testresults
    where (s,d) = getSD (n-1)
          tlist = [ modPow n a (2^r * d) | r <- [0..s-1] ]
          testresults = (modPow n a d == 1):(map (== n-1) tlist)
          
getSD :: Integer -> (Integer,Integer)
getSD n
    | even n    = (x+1,y)
    | otherwise = (1,n)
    where (x,y) = getSD (n `div` 2)

jacobiSymbol :: Integer -> Integer -> Integer
jacobiSymbol a n
    | a <= 0       = jacobiSymbol (a+n) n
    | gcd a n /= 1 = 0
    | a == 1       = 1
    | n == 1       = 1
    | a == 2       = if (n `mod` 8) `elem` [1,7] then 1 else -1
    | a > n        = jacobiSymbol (a `mod` n) n
    | even a       = (jacobiSymbol 2 n) * (jacobiSymbol (a `div` 2) n)
    | otherwise    = k * jacobiSymbol n a
    where k = if (n `mod` 4 == 1) || (a `mod` 4 == 1) then 1 else -1


lucasPrimeTest :: Integer -> Bool
lucasPrimeTest 2 = True
lucasPrimeTest n
    | n < 2                 = False
    | even n                = False
    | (floorSqrt n)^2 == n  = False
    | gcd q n /= 1          = False
    | jacobiSymbol d n == 0 = False
    | otherwise             = p1 || p2
    where dOpts = zipWith (*) [5,7..] $ cycle [1,-1]
          d = head . filter (\x -> jacobiSymbol x n <= 0) $ dOpts
          q = (1 - d) `div` 4
          p1 = fst (lucasNumber d' n (d,1,q)) == 0 
          p2 = any (\k -> snd (lucasNumber k n (d,1,q)) == 0) $ map (\r -> d' * modPow n 2 r) rs
          (s,d') = getSD (n+1)
          rs = [0..s-1]

lucasNumber n m (d,p,q) = foldr ($) (1,p) indices
    where indices = unfoldr doubleOrAddOne n
          doubleOrAddOne k
              | k == 1    = Nothing
              | even k    = Just (double, (k `div` 2))
              | otherwise = Just (addOne, (k - 1))
          double (u,v) = (u*v `mod` m, half (v*v + d*u*u))
          addOne (u,v) = (half (p*u + v), half (d*u + p*v))
          half k = if even k then k `div` 2 `mod` m else (k+m) `div` 2 `mod` m

baillePSWTest :: Integer -> Bool
baillePSWTest n
    | n < 2  = False
    | n == 2 = True
    | even n = False
    | cs smallPrimes            = False
    | not (millerRabinTest 2 n) = False
    | otherwise                 = lucasPrimeTest n
    where cs [] = False
          cs (x:xs)
            | n == x         = False
            | n `mod` x == 0 = True
            | otherwise      = cs xs



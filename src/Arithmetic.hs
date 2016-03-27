module Arithmetic where

import Data.Maybe
import Data.List (find)
import MyLists (pack)

isPrime :: Int -> Bool
isPrime n = foldl (\acc i -> (acc && (n `mod` i /= 0))) True [2..floor(sqrt(fromIntegral n))]

gcd' :: Int -> Int -> Int
gcd' x y
    | x <=0 || y <= 0 = error "GCD of non-positive integers cannot be computed"
    | otherwise       = let a = (min x y)
                            b = (max x y) in
                        if b `mod` a == 0 then a else (gcd' a (b `mod` a))

coprime :: Int -> Int -> Bool
coprime x y = (gcd' x y) == 1

totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..(n - 1)]


primeFactors :: Int -> [Int]
primeFactors n = case (find (\k -> (isPrime k) && (n `mod` k == 0)) [2..floor(sqrt(fromIntegral n))]) of
    Just a  -> a : (primeFactors (n `quot` a))
    Nothing -> [n]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (\ls -> (head ls, length ls)) . pack . primeFactors

phi :: Int -> Int
phi n = product [(elem - 1) * elem ^ (count - 1) | (elem, count) <- primeFactorsMult n]

primesR :: Int -> Int -> [Int]
primesR b e = [k | k <- [b..e], (isPrime k)]

goldbach :: Int -> (Int, Int)
goldbach n = case (find (\k -> (isPrime k) && (isPrime (n - k))) [3..(n `div` 2)]) of
             Just a -> (a, n - a)
             Nothing -> error "Goldbachs conjecture is wrong!!"

goldbachList :: Int -> Int -> IO ()
goldbachList a b =
    mapM_ (\j -> (print ((show j) ++ " " ++ (show (goldbach j))))) [i | let c = (if (a `mod` 2) == 0 then a else a + 1), i <- [c,(c + 2)..b]]


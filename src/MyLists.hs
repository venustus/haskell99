module MyLists where

import System.Random hiding (split)
import Control.Monad (replicateM)
import Data.List
import qualified Data.Map as Map

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast (x : []) = x
myLast (_ : xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "No but last for empty lists!"
myButLast (x : []) = error "No but last for singleton lists!"
myButLast (x : y : []) = x
myButLast (x : y : xs) = myButLast (y : xs)

elementAt :: [a] -> Int -> a
elementAt [] _ = error "No kth element of an empty list!"
elementAt (x : xs) k
    | k < 1     = error "Index out of bounds"
    | k == 1    = x
    | otherwise = elementAt xs (k - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

data NestedList a = Elem a | List [NestedList a] deriving (Show)

-- flattens a nested list and returns a haskell list of all containing elements
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x : xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- compresses a list of elements such that consecutive elements are singled out
compress :: (Eq a) => [a] -> [a]
compress [x] = [x]
compress [] = []
compress (x : y : xs)
    | x == y    = compress (x : xs)
    | otherwise = x : compress (y : xs)

-- packs equal consecutive elements into sublists and returns a list of lists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : y : xs)
    | x /= y    = [x] : pack (y : xs)
    | otherwise = let xsp = pack (y : xs) in
                  (x : head xsp) : tail xsp

-- packs equal consecutive elements into tuples of length and element
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\ls -> (length ls, head ls)) . pack

data PackedListElem a = Single a | Multiple Int a deriving (Show)

-- same as encode, but if an element has no duplicates, it is simply copied to the result list
encodeModified :: (Eq a) => [a] -> [PackedListElem a]
encodeModified xs = let encodedList = encode xs in
                     map (\(i, elem) -> if i == 1 then (Single elem) else (Multiple i elem)) encodedList

-- opposite of encodeModified
decodeModified :: (Eq a) => [PackedListElem a] -> [a]
decodeModified []                  = []
decodeModified ((Single x) : xs)     = x : decodeModified xs
decodeModified ((Multiple i x) : xs)
    | i == 1    = x : (decodeModified xs)
    | otherwise = x : (decodeModified ((Multiple (i - 1) x) : xs))

-- same as encode, but implemented directly
encodeDirect :: (Eq a) => [a] -> [PackedListElem a]
encodeDirect []       = []
encodeDirect [x]      = [(Single x)]
encodeDirect (x : xs) = let y = encodeDirect xs in
                        case y of
                            ((Single z) : zs)     -> if x == z
                                                     then (Multiple 2 x) : zs
                                                     else (Single x) : y
                            ((Multiple i z) : zs) -> if x == z
                                                     then (Multiple (i + 1) x) : zs
                                                     else (Single x) : y

dupli :: [a] -> [a]
dupli []     = []
dupli (x : xs) = [x, x] ++ dupli xs

repli :: [a] -> Int -> [a]
repli [] _       = []
repli (x : xs) i = map (\_ -> x) [1..i] ++ (repli xs i)

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs i = let l = length xs in
                 if l >= i then (take (i - 1) xs) ++ (dropEvery (drop i xs) i)
                 else xs


split :: [a] -> Int -> ([a], [a])
split [] i
    | i > 0     = error "Invalid split length"
    | otherwise = ([], [])
split ls@(x : xs) i
    | i == 0 = ([], ls)
    | otherwise = let (ys, zs) = split xs (i - 1) in
                  (x : ys, zs)


slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice ls@(x : xs) i k
    | i <= 1 && k >= 1 = x : ys
    | otherwise        = ys
    where ys = slice xs (i - 1) (k - 1)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate ls 0 = ls
rotate ls i
    | i > 0     = (drop i ls) ++ (take i ls)
    | otherwise = (drop (l + i) ls) ++ (take (l + i) ls)
    where l = length ls

removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Cannot remove anything from empty list"
removeAt i (x : xs)
    | i == 1 = (x, xs)
    | i > 1  = let (y, ys) = removeAt (i - 1) xs in (y, x : ys)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = (take (i - 1) xs) ++ (x : (drop (i - 1) xs))

range :: Int -> Int -> [Int]
range i j
    | i > j     = []
    | otherwise = i : (range (i + 1) j)


rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return $ xs !! r

diff_select :: [a] -> Int -> IO [a]
diff_select xs n
    | n == 0    = do return []
    | otherwise = if (n > (length xs))
                  then error "Not enough numbers in the list"
                  else do r <- randomRIO(0, (length xs) - 1)
                          rest <- diff_select ((take r xs) ++ (drop (r + 1) xs)) (n - 1)
                          return $ (xs !! r) : rest


rnd_permute :: [a] -> IO [a]
rnd_permute [] = do return []
rnd_permute xs = do r <- randomRIO(0, (length xs) - 1)
                    rest <- rnd_permute ((take r xs) ++ (drop (r+1) xs))
                    return $ (xs !! r) : rest


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [(xs !! i) : xi | i <- [0..((length xs) - 1)], xi <- (combinations (n - 1) (drop (i+1) xs))]

lsort :: [[a]] -> [[a]]
lsort = sortOn length

lfsort :: [[a]] -> [[a]]
lfsort ls =  let freqMap = Map.fromListWith (+) (map (\x -> ((length x), 1)) ls) in
             sortOn (\l -> (freqMap Map.! (length l))) ls


factlist = 1 : (zipWith (*) [1..] factlist)
factl n = factlist !! n

fibs = 1:1:zipWith (+) fibs (tail fibs)
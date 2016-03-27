module BinaryTrees where

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)


cbalTree :: (Eq a) => Int -> a -> [Tree a]
cbalTree 0 _ = [Empty]
cbalTree 1 x = [(Branch x Empty Empty)]
cbalTree n x
    | ((n - 1) `mod` 2) == 0 = [(Branch x y z) | let a = (cbalTree ((n - 1) `div` 2) x), y <- a, z <- a]
    | otherwise              = [(Branch x y z) | y <- a, z <- b] ++ [(Branch x y z) | y <- b, z <- a]
                               where
                                   a = (cbalTree ((n - 1) `div` 2) x)
                                   b = (cbalTree (((n - 1) `div` 2) + 1) x)
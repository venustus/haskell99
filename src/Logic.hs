module Logic where

import Data.Heap

and' :: Bool -> Bool -> Bool
and' a b  = if a then b else a

or' :: Bool -> Bool -> Bool
or' a b
    | a         = True
    | b         = True
    | otherwise = False

neg' :: Bool -> Bool
neg' a = if a then False else True

nand' :: Bool -> Bool -> Bool
nand' a b = neg' $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = neg' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' a b = (a `and'` (neg' b)) `or'` ((neg' a) `and'` b)

impl' :: Bool -> Bool -> Bool
impl' a b = (neg' a) `or'` b

equ' :: Bool -> Bool -> Bool
equ' a b = (a == b)

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [(show a) ++ " " ++ (show b) ++ " " ++ (show c) | a <- [True, False], b <- [True, False], let c = (f a b)]

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

genTrueFalseLists :: Int -> [[Bool]]
genTrueFalseLists 0 = [[]]
genTrueFalseLists n = [x : xs | x <- [True, False], xs <- (genTrueFalseLists (n - 1))]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [((foldl (\acc v -> acc ++ " " ++ (show v)) "" ls) ++ " " ++ (show res)) | ls <- (genTrueFalseLists n), let res = (f ls)]

gray :: Int -> [[Char]]
gray 1 = ["0", "1"]
gray n = [x ++ xs | x <- ["0", "1"], xs <- gray (n - 1)]

memoized_gray :: Int -> [[Char]]
memoized_gray = (map gray' [0 ..] !!)
    where gray' 0 = []
          gray' 1 = ["0", "1"]
          gray' n = [x ++ xs | x <- ["0", "1"], xs <- memoized_gray (n - 1)]

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

data PrefixTree a = PTLeaf Int a | PTNode Int (PrefixTree a) (PrefixTree a) deriving (Show, Eq)

instance Eq a => Ord (PrefixTree a) where
    (PTLeaf i1 _) <= (PTLeaf i2 _) = i1 <= i2
    (PTLeaf i1 _) <= (PTNode i2 _ _) = i1 <= i2
    (PTNode i1 _ _) <= (PTNode i2 _ _) = i1 <= i2
    (PTNode i1 _ _) <= (PTLeaf i2 _) = i1 <= i2

merge_prefix_tree :: PrefixTree a -> PrefixTree a -> PrefixTree a
merge_prefix_tree left@(PTLeaf i1 _) right@(PTLeaf i2 _) = PTNode (i1 + i2) left right
merge_prefix_tree left@(PTLeaf i1 _) right@(PTNode i2 _ _) = PTNode (i1 + i2) left right
merge_prefix_tree left@(PTNode i1 _ _) right@(PTNode i2 _ _) = PTNode (i1 + i2) left right
merge_prefix_tree left@(PTNode i1 _ _) right@(PTLeaf i2 _) = PTNode (i1 + i2) left right

create_huffman_tree :: [(a, Int)] -> PrefixTree a
create_huffman_tree ls = viewHead pq
    where pq = fromList (map (\p -> case p of (c, i) -> PTLeaf i c) ls) :: MinHeap (PrefixTree a)


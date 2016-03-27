{-# LANGUAGE ScopedTypeVariables #-}

module Logic where

import Data.Heap
import qualified Data.Map as Map

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

create_huffman_tree_from_nodes :: (Ord a) => MinHeap (PrefixTree a) -> PrefixTree a
create_huffman_tree_from_nodes pq = case (min_elem, second_min_elem, rest_of_heap) of
        (Just min_node, Just second_min_node, rpq) -> create_huffman_tree_from_nodes $ (insert (merge_prefix_tree min_node second_min_node) rpq)
        (Just min_node, Nothing, _) -> min_node
        (Nothing, _, _) -> error "Invalid input - heap is empty"
        where split_heap = view pq
              (min_elem, second_min_elem, rest_of_heap) = case split_heap of
                Just (min, rest) -> case (view rest) of
                    Just(second_min, final_rest) -> (Just min, Just second_min, final_rest)
                    Nothing -> (Just min, Nothing, rest)
                Nothing -> (Nothing, Nothing, pq)

create_huffman_tree :: forall a. (Ord a) => [(a, Int)] -> PrefixTree a
create_huffman_tree ls = create_huffman_tree_from_nodes $ (fromList (map (\p -> case p of (c, i) -> PTLeaf i c) ls) :: MinHeap (PrefixTree a))

create_huffman_table :: (Ord a) => (PrefixTree a) -> [Char] -> Map.Map a [Char]
create_huffman_table (PTLeaf n x) str = Map.insert x str Map.empty
create_huffman_table (PTNode n left right) str = Map.union (create_huffman_table left (str ++ ['0'])) (create_huffman_table right (str ++ ['1']))

encode_huffman :: (Ord a) => Map.Map a [Char] -> [a] -> [Char]
encode_huffman table = foldl (\acc l -> acc ++ (Map.findWithDefault "NULL" l table)) ""

decode_huffman_single :: (Ord a) => PrefixTree a -> [Char] -> ([Char], a)
decode_huffman_single (PTLeaf _ x) str = (str, x)
decode_huffman_single (PTNode _ _ _) [] = error "Invalid encoding"
decode_huffman_single (PTNode _ left right) str@(c : cs)
    | c == '1' = decode_huffman_single right cs
    | c == '0' = decode_huffman_single left cs

decode_huffman :: (Ord a) => PrefixTree a -> [Char] -> [a]
decode_huffman tree [] = []
decode_huffman tree cs = let (rest, current) = decode_huffman_single tree cs in
    current : (decode_huffman tree rest)





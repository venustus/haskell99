module MultiWayTrees where

import Debug.Trace (trace)

data Tree a = Node a [Tree a]
    deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node r [])       = 1
nnodes (Node r children) = 1 + (foldl (\acc x -> acc + (nnodes x)) 0 children)

treeToString :: Tree Char -> [Char]
treeToString (Node r []) = r : "^"
treeToString (Node r xs) = (r : (foldl (\acc x -> acc ++ (treeToString x)) [] xs)) ++ "^"

-- converts a string to a multi-way tree
stringToTree :: [Char] -> (Tree Char, [Char])
-- stringToTree s | trace ("stringToTree " ++ show s) False = undefined
stringToTree (s : rest) = let (first, second) = stringToTrees rest s [] in
                          (Node s first, second)

-- extracts a set of children from a string representation of a multi-way tree
-- stops when children of current root are finished
stringToTrees :: [Char] -> Char -> [Tree Char] -> ([Tree Char], [Char])
-- stringToTrees s rootChar childList | trace ("stringToTreeHelper " ++ show s ++ show rootChar ++ show childList) False = undefined
stringToTrees ('^' : rest) rootChar childList = (childList, rest)
stringToTrees s rootChar childList            = let (firstChild, t) = (stringToTree s) in
                                                (stringToTrees t rootChar (childList ++ [firstChild]))


-- computes internal path length of a tree
-- internal path length of a tree is sum of number of edges from root to each node
-- in the tree
ipl :: (Show a) => Tree a -> Int -> Int
-- ipl t h | trace ("ipl " ++ (show t) ++ " " ++ (show h)) False = undefined
ipl (Node x []) h = h
ipl (Node x xs) h = foldl (\acc l -> acc + (ipl l (h + 1))) h xs

bottomUp :: Tree Char -> [Char]
bottomUp (Node x []) = [x]
bottomUp (Node x xs) = (foldl (\acc l -> acc ++ (bottomUp l)) "" xs) ++ [x]

lisp :: Tree Char -> [Char]
lisp (Node x []) = [x]
lisp (Node a xs) = "(" ++ [a] ++ " " ++ (unwords (map lisp xs)) ++ ")"
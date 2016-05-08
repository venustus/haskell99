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

mirror :: (Eq a) => (Tree a) -> (Tree a) -> Bool
mirror Empty Empty = True
mirror (Branch x left1 right1) (Branch y left2 right2) = (mirror left1 right2) && (mirror right1 left2)
mirror _ _ = False

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch x left right) = mirror left right

addToTree :: (Ord a) => a -> Tree a -> Tree a
addToTree x Empty = Branch x Empty Empty
addToTree x t@(Branch y left right)
    | x == y    = t
    | x < y     = Branch y (addToTree x left) right
    | otherwise = Branch y left (addToTree x right)

constructBinarySearchTree xs = foldl (flip addToTree) Empty xs

symCbalTrees :: (Eq a) => Int -> a -> [Tree a]
symCbalTrees n x = filter symmetric $ cbalTree n x

hbalTree :: (Eq a) => Int -> a -> [Tree a]
hbalTree 0 _ = [Empty]
hbalTree 1 x = [(Branch x Empty Empty)]
hbalTree h x = [(Branch x left right) | left <- a, right <- b] ++ [(Branch x left right) | left <- b, right <- a] ++ [(Branch x left right) | left <- a, right <- a]
                where
                    a = (hbalTree (h - 1) x)
                    b = (hbalTree (h - 2) x)

hbalTreeEfficient :: (Eq a) => a -> Int -> [Tree a]
hbalTreeEfficient x = (map (hbalTree' x) [0 ..] !!)
                      where
                        hbalTree' _ 0 = [Empty]
                        hbalTree' x 1 = [(Branch x Empty Empty)]
                        hbalTree' x h = [(Branch x left right) | left <- a, right <- b] ++ [(Branch x left right) | left <- b, right <- a] ++ [(Branch x left right) | left <- a, right <- a]
                                                        where
                                                            a = (hbalTreeEfficient x (h - 1))
                                                            b = (hbalTreeEfficient x (h - 2))


hbaltree :: a -> Int -> [Tree a]
hbaltree x h = trees !! h
  where trees = [Empty] : [Branch x Empty Empty] :
                zipWith combine (tail trees) trees
        combine ts shortts = [Branch x l r |
                (ls, rs) <- [(shortts, ts), (ts, ts), (ts, shortts)],
                l <- ls, r <- rs]


minNodes :: Int -> Int
minNodes 1 = 1
minNodes 2 = 2
minNodes h = 1 + min (a + b) (2 * a)
    where a = minNodes (h - 1)
          b = minNodes (h - 2)

maxHeight :: Int -> Int
maxHeight 0 = 0
maxHeight 1 = 1
maxHeight 2 = 2
maxHeight n = 1 + maximum (map (\(x, y) -> max x y) l)
    where l = [(x, y) | a <- [0 .. (n - 1)], b <- [0 .. (n - 1)], a + b == (n - 1),
                let x = maxHeight a, let y = maxHeight b, abs(x - y) <= 1]


minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n + 1)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch x l r) = 1 + (countNodes l) + (countNodes r)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = concatMap (\h -> (filter (\t -> (n == (countNodes t))) (hbaltree x h))) [(minHeight n) .. (maxHeight n)]


countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch x Empty Empty) = 1
countLeaves (Branch x left right) = (countLeaves left) + (countLeaves right)

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch x left right) = (leaves left) ++ (leaves right)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x left right) = [x] ++ (internals left) ++ (internals right)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch x left right) n = (atLevel left (n - 1)) ++ (atLevel right (n - 1))

constructCompleteBinaryTree' :: Int -> Int -> a -> Tree a
constructCompleteBinaryTree' n i x
    | 2 * i <= n && (2 * i + 1) <= n = Branch x (constructCompleteBinaryTree' n (2 * i) x) (constructCompleteBinaryTree' n (2 * i + 1) x)
    | 2 * i <= n && (2 * i + 1) > n = Branch x (constructCompleteBinaryTree' n (2 * i) x) Empty
    | otherwise = Branch x Empty Empty

constructCompleteBinaryTree :: Int -> a -> Tree a
constructCompleteBinaryTree n = constructCompleteBinaryTree' n 1
{-# LANGUAGE DeriveGeneric #-}

module Graphs where

import Prelude hiding (cycle)
import Data.Set (Set, insert, fromList, toList, empty, member, singleton, size)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Debug.Trace (trace)
import UnionFind (newUnionFindDataSet, union, find)
import Data.List (sortBy, minimumBy)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Vertex a = Vertex a deriving (Generic)

instance Hashable a => Hashable (Vertex a)

deriving instance Eq a => Eq (Vertex a)
deriving instance Ord a => Ord (Vertex a)
deriving instance Show a => Show (Vertex a)

data Edge a b = Edge (Vertex a) (Vertex a) b

deriving instance (Eq a, Eq b) => Eq (Edge a b)
deriving instance (Ord a, Ord b) => Ord (Edge a b)
deriving instance (Show a, Show b) => Show (Edge a b)

data Graph a b = TermGraph (Set (Vertex a)) (Set (Edge a b)) |
                 AdjGraph (Map.Map (Vertex a) [(Vertex a, b)])
                 deriving (Show)

termGraphFromList :: (Ord a, Ord b) => [a] -> [(a, a, b)] -> Graph a b
termGraphFromList vertices edges = TermGraph (fromList (map (\v -> Vertex v) vertices)) (fromList (map (\(x, y, p) -> Edge (Vertex x) (Vertex y) p) edges))

undirectedTermGraphFromList :: (Ord a, Ord b) => [a] -> [(a, a, b)] -> Graph a b
undirectedTermGraphFromList vertices edges =
    TermGraph (fromList (map (\v -> Vertex v) vertices)) (fromList (concat (map (\(x, y, p) -> [Edge (Vertex x) (Vertex y) p, Edge (Vertex y) (Vertex x) p]) edges)))

convertFromTermGraphToAdjGraph :: (Ord a, Ord b) => (Graph a b) -> (Graph a b)
convertFromTermGraphToAdjGraph (TermGraph vertices edges) =
    AdjGraph $ foldl (\acc v -> Map.insertWith (\_ old -> old) v [] acc) (foldl (\acc (Edge x y p)  ->  Map.insertWith (\new old -> old ++ new) x [(y, p)] acc) Map.empty edges) vertices
convertFromTermGraphToAdjGraph _ = error "Illegal input format"

convertFromAdjGraphToTermGraph :: (Ord a, Ord b) => (Graph a b) -> (Graph a b)
convertFromAdjGraphToTermGraph (AdjGraph m) = TermGraph (Map.keysSet m) (Map.foldrWithKey (\k v acc -> (foldl (\acc2 (y, p) -> (insert (Edge k y p) acc2)) acc v)) empty m)
convertFromAdjGraphToTermGraph _ = error "Illegal input format"

paths :: (Ord a, Ord b) => Vertex a -> Vertex a -> (Graph a b) -> [[Vertex a]]
paths x y g = paths1 x y g empty

paths1 :: (Ord a, Ord b) => Vertex a -> Vertex a -> (Graph a b) -> (Set (Vertex a)) -> [[Vertex a]]
paths1 x y t@(TermGraph _ _) visited            = paths1 x y (convertFromTermGraphToAdjGraph t) visited
paths1 x y g@(AdjGraph vertexMap) visited       =
    if x == y then [[y]] else
        if (member x visited) then [] else
         case (Map.lookup x vertexMap) of
             Nothing         -> error "Vertex not present in graph"
             Just neighbors  ->
                (foldl (\acc v -> [x : p | p <- (paths1 (fst v) y g (insert x visited))] ++ acc) [] neighbors)


cycle :: (Show a, Show b, Ord a, Ord b) => Vertex a -> (Graph a b) -> [[Vertex a]]
cycle s t@(TermGraph _ _) = cycle s (convertFromTermGraphToAdjGraph t)
cycle s g@(AdjGraph vMap) = case (Map.lookup s vMap) of
                                Nothing        -> error ("Vertex " ++ (show s) ++ " not present in graph " ++ (show vMap))
                                Just neighbors ->
                                    (foldl (\acc v -> [s : p | p <- (paths (fst v) s g)] ++ acc) [] neighbors)

subsets :: [a] -> [[a]]
subsets []       = [[]]
subsets (x : xs) = let res = subsets xs in
                        res ++ map (x :) res

generateAllSpanningTrees :: (Show a, Show b, Ord a, Ord b) => (Graph a b) -> [(Graph a b)]
generateAllSpanningTrees g@(AdjGraph _) = generateAllSpanningTrees (convertFromAdjGraphToTermGraph g)
generateAllSpanningTrees t@(TermGraph vertices edges) =
    filter (not . hasCycles) $ filter spanning allTrees
    where
        allTrees = map (\le -> TermGraph vertices (fromList le)) (subsets (toList edges))
        spanning (TermGraph _ es) = (vertices == fromList (concat (map (\(Edge va vb l) -> [va, vb]) (toList es))))
        hasCycles t@(TermGraph vs es) = any ((/=) 0 . length) $ map (\v -> cycle v t) (toList vs)


-- Implements Kruskal's greedy algorithm to return minimum spanning tree
-- of a given graph. Uses UnionFind data set.
getMinimumSpanningTreeKruskal :: (Hashable a, Show a, Show b, Ord a, Ord b) => (Graph a b) -> [Edge a b]
getMinimumSpanningTreeKruskal g@(AdjGraph _) = getMinimumSpanningTreeKruskal (convertFromAdjGraphToTermGraph g)
getMinimumSpanningTreeKruskal t@(TermGraph vertices edges) =
    let ufds = newUnionFindDataSet vertices in
    snd $ foldl (\(uf, es) e@(Edge a b l) ->
              let (leader1, uf1) = find a uf
                  (leader2, uf2) = find b uf1 in
              if leader1 /= leader2 then (union a b uf2, e : es) else (uf2, es)
          ) (ufds, []) $ sortBy (\(Edge _ _ e1) (Edge _ _ e2) -> compare e1 e2) $ toList edges

getMinimumSpanningTreePrim :: (Show a, Show b, Ord a, Ord b) => (Graph a b) -> [Edge a b]
getMinimumSpanningTreePrim g@(AdjGraph _) = getMinimumSpanningTreePrim (convertFromAdjGraphToTermGraph g)
getMinimumSpanningTreePrim t@(TermGraph vertices edges) =
    spanningTreeHelper [] $ singleton $ head $ toList vertices
    where
        spanningTreeHelper ansAcc consumedVertices =
            if (size consumedVertices) == (size vertices) then ansAcc else
            let safeEdge@(Edge v1 v2 _) = minimumBy (\(Edge _ _ e1) (Edge _ _ e2) -> compare e1 e2) $
                           filter (\(Edge a b _) -> (member a consumedVertices) && (not (member b consumedVertices))) $
                           toList edges in
            spanningTreeHelper (safeEdge : ansAcc) (insert v2 consumedVertices)

depthFirst :: (Show a, Show b, Ord a, Ord b) => Vertex a -> (Graph a b) -> [Vertex a]
depthFirst s t@(TermGraph _ _) = depthFirst s (convertFromTermGraphToAdjGraph t)
depthFirst s g@(AdjGraph vMap)    = snd $ dfHelper empty s
    where
        dfHelper visited start =
            if (member start visited) then (visited, [])
            else case (Map.lookup start vMap) of
                 Nothing -> (visited, [start])
                 Just neighbors ->
                    let (newVisited, ansAcc) = foldl (\(vis, ans) neighbor -> let (interVis, interAns) = (dfHelper vis neighbor) in (interVis, ans ++ interAns)) ((insert start visited), []) $ map fst neighbors in
                    (newVisited, start : ansAcc)

module UnionFind where

import qualified Data.HashMap as Map
import Data.Set (Set)
import Data.Hashable (Hashable)

data UnionFind a = UnionFind (Map.Map a a) (Map.Map a Int) deriving (Eq, Show)

newUnionFindDataSet :: (Hashable a, Ord a, Show a) => Set a -> UnionFind a
newUnionFindDataSet s = let parentMap = foldl (\acc selem -> (Map.insert selem selem acc)) Map.empty s
                            rankMap = foldl (\acc selem -> (Map.insert selem 1 acc)) Map.empty s in
                        UnionFind parentMap rankMap

findPath :: (Hashable a, Ord a, Show a) => a -> UnionFind a -> [a] -> [a]
findPath x uf ls = case uf of
                   UnionFind parentMap rankMap -> case (Map.lookup  x parentMap) of
                                                  Just p  -> if p == x then x : ls else (findPath p uf (x : ls))
                                                  Nothing -> error "Illegal argument"

find :: (Hashable a, Ord a, Show a) => a -> UnionFind a -> (a, UnionFind a)
find x uf = let pathToLeader = findPath x uf []
                leader       = head pathToLeader in
            if (length pathToLeader) <= 2 then (leader, uf)
            else case uf of
                 UnionFind parentMap rankMap -> (
                                                    head pathToLeader,
                                                    UnionFind (foldl (\acc e -> (Map.insert e leader acc)) parentMap (tail (tail pathToLeader))) rankMap
                                                )

union :: (Hashable a, Ord a, Show a) => a -> a -> UnionFind a -> UnionFind a
union x y uf = let (leader1, uf1) = find x uf
                   (leader2, uf2) = find y uf1 in
               case uf2 of
                   UnionFind pMap rMap ->
                      (let r1                                    = Map.findWithDefault 0 leader1 rMap
                           r2                                    = Map.findWithDefault 0 leader2 rMap
                           sameRanks                             = r1 == r2
                           (largerRankLeader, smallerRankLeader) = if(r1 > r2) then (leader1, leader2) else (leader2, leader1) in
                       UnionFind (Map.insert smallerRankLeader largerRankLeader pMap) (if sameRanks then (Map.insert largerRankLeader (r1 + 1) rMap) else rMap))


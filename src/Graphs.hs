module Graphs where

import Data.Set (Set, insert, fromList, empty)
import qualified Data.Map as Map

data Vertex a = Vertex a

deriving instance Eq a => Eq (Vertex a)
deriving instance Ord a => Ord (Vertex a)
deriving instance Show a => Show (Vertex a)

data Edge a b = Edge (Vertex a) (Vertex a) b

deriving instance (Eq a, Eq b) => Eq (Edge a b)
deriving instance (Ord a, Ord b) => Ord (Edge a b)
deriving instance (Show a, Show b) => Show (Edge a b)

data Graph a b = TermGraph (Set (Vertex a), Set (Edge a b)) |
                 AdjGraph (Map.Map (Vertex a) [(Vertex a, b)])
                 deriving (Show)

termGraphFromList :: (Ord a, Ord b) => [a] -> [(a, a, b)] -> Graph a b
termGraphFromList vertices edges = TermGraph (fromList (map (\v -> Vertex v) vertices), fromList (map (\(x, y, p) -> Edge (Vertex x) (Vertex y) p) edges))

convertFromTermGraphToAdjGraph :: (Ord a, Ord b) => (Graph a b) -> (Graph a b)
convertFromTermGraphToAdjGraph (TermGraph (vertices, edges)) =
    AdjGraph (foldl (\acc (Edge x y p)  ->  Map.insertWith (\new old -> new ++ old) x [(y, p)] acc) Map.empty edges)
convertFromTermGraphToAdjGraph _ = error "Illegal input format"

convertFromAdjGraphToTermGraph :: (Ord a, Ord b) => (Graph a b) -> (Graph a b)
convertFromAdjGraphToTermGraph (AdjGraph m) = TermGraph (Map.keysSet m, Map.foldrWithKey (\k v acc -> (foldl (\acc2 (y, p) -> (insert (Edge k y p) acc2)) acc v)) empty m)
convertFromAdjGraphToTermGraph _ = error "Illegal input format"
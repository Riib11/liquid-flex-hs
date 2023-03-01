module Graph where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M

type Graph a = Map a [a]

type Edge a = (a, a)

vertices :: Graph a -> [a]
vertices g = M.keys g

edges :: Ord a => Graph a -> a -> [a]
edges g v = case M.lookup v g of
  Nothing -> []
  Just vs -> vs

allEdges :: Graph a -> [Edge a]
allEdges g = [(u, v) | (u, vs) <- M.toList g, v <- vs]

buildG :: Ord a => [Edge a] -> Graph a
buildG es = M.fromListWith (++) [(u, [v]) | (u, v) <- es]

transposeG :: Ord a => Graph a -> Graph a
transposeG g = buildG $ map flipEdge (allEdges g)

flipEdge :: Edge a -> Edge a
flipEdge (u, v) = (v, u)

outdegree :: Graph a -> Map a Int
outdegree = fmap length

indegree :: Ord a => Graph a -> Map a Int
indegree g = outdegree $ transposeG g

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

type Forest a = [Tree a]

data Tree a = Node a (Forest a)
  deriving (Read, Show)

type State s a = s -> (a, s)

type Marks a = Map a ()

dff :: Ord a => Graph a -> Forest a
dff g = dfs g (vertices g)

dfs :: Ord a => Graph a -> [a] -> Forest a
dfs g vs = fst $ search g vs M.empty

search :: Ord a => Graph a -> [a] -> State (Marks a) (Forest a)
search _ [] m = ([], m)
search g (v : vs) m =
  case M.lookup v m of
    Just _ -> search g vs m
    Nothing ->
      let m1 = M.alter (const $ Just ()) v m
          (ts, m2) = search g (edges g v) m1
          (us, m3) = search g vs m2
       in (Node v ts : us, m3)

-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

preOrderF :: Forest a -> [a]
preOrderF ts = preOrderF' ts []

preOrderF' :: Forest a -> [a] -> [a]
preOrderF' ts = foldr (.) id $ map preOrderT' ts

preOrderT :: Tree a -> [a]
preOrderT t = preOrderT' t []

preOrderT' :: Tree a -> [a] -> [a]
preOrderT' (Node a ts) = (a :) . preOrderF' ts

postOrderF :: Forest a -> [a]
postOrderF ts = postOrderF' ts []

postOrderF' :: Forest a -> [a] -> [a]
postOrderF' ts = foldr (.) id $ map postOrderT' ts

postOrderT' :: Tree a -> [a] -> [a]
postOrderT' (Node a ts) = postOrderF' ts . (a :)

components :: Ord a => Graph a -> [[a]]
components g = map preOrderT (dff (undirected g))

undirected :: Ord a => Graph a -> Graph a
undirected g = buildG $ nub (es ++ map flipEdge es)
  where
    es = allEdges g

-- A topological sort of the graph

topSort :: Ord a => Graph a -> [a]
topSort g = reverse (postOrderF (dff g))

topSortRoots :: Ord a => Graph a -> [a] -> [a]
topSortRoots g roots = reverse (postOrderF (dfs g roots))

-- Strongly connected components

scc :: Ord a => Graph a -> [[a]]
scc g = map preOrderT (dfs g (topSort (transposeG g)))

reachable :: Ord a => Graph a -> a -> [a]
reachable g v = preOrderF (dfs g [v])

path :: Ord a => Graph a -> a -> a -> Bool
path g v w = w `elem` (reachable g v)

-----------------------------------
-- Test graphs --------------------

g1 :: Graph Integer
g1 = buildG [(1, 2), (1, 3), (2, 3)]

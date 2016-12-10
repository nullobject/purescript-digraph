module Data.Graph
  ( AdjacencyList
  , Graph
  , fromAdjacencyList
  , vertices
  , size
  , elem
  , adjacent
  , adjacent'
  , isAdjacent
  , weight
  , shortestPath
  , shortestPath'
  , traverse
  , connectedComponents
  ) where

import Prelude

import Data.Foldable (elem, foldl) as F
import Data.List (List(..), (\\))
import Data.List (filter, reverse, singleton, snoc) as L
import Data.Map (Map)
import Data.Map (alter, empty, insert, keys, lookup, member, singleton, size, toList) as M
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Set (Set)
import Data.Set (empty, insert, member) as S
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

import Data.PQueue (PQueue)
import Data.PQueue (insert, isEmpty, singleton) as PQ
import Data.PQueue.Partial (head, tail) as PPQ

infixr 6 Cons as :

type AdjacencyList a w = List (Tuple a (List (Tuple a w)))

newtype Graph a w = Graph (Map a (Map a w))

derive instance newtypeGraph :: Newtype (Graph a w) _

instance showGraph :: (Show a, Show w) => Show (Graph a w) where
  show = show <<< unwrap

-- Create a graph from an adjacency list.
fromAdjacencyList :: forall a w. (Ord a) => AdjacencyList a w -> Graph a w
fromAdjacencyList as = wrap adjacencyMap
  where
    -- Create an empty adjacency map for the vertices.
    emptyAdjacencyMap = F.foldl (\m (Tuple a _) -> M.insert a M.empty m) M.empty as

    -- Calculates the adjacency map by mapping vertices to adjacent vertices.
    adjacencyMap = F.foldl (flip insertVertex) emptyAdjacencyMap as

    insertVertex (Tuple a edges) m = F.foldl (flip $ insertEdge a) m edges

    -- insertEdge a (Tuple b w) = M.alter (upsert a w) b <<< M.alter (upsert b w) a
    insertEdge a (Tuple b w) = M.alter (upsert a w) b

    -- If the map has no value then add a singleton map. Otherwise insert the
    -- value into the existing map.
    upsert a w Nothing = Just (M.singleton a w)
    upsert a w (Just es) = Just (M.insert a w es)

-- Get the vertices of a graph.
vertices :: forall a w. Graph a w -> List a
vertices = M.keys <<< unwrap

-- Get the number of vertices in a graph.
size :: forall a w. Graph a w -> Int
size = M.size <<< unwrap

-- Test whether a vertex is in a graph.
elem :: forall a w. (Ord a) => a -> Graph a w -> Boolean
elem vertex = M.member vertex <<< unwrap

-- Get the adjacent vertices of a vertex.
adjacent :: forall a w. (Ord a) => a -> Graph a w -> List a
adjacent vertex graph = maybe Nil M.keys (M.lookup vertex (unwrap graph))

-- Get the adjacent vertices and associated costs of a vertex.
adjacent' :: forall a w. (Ord a) => a -> Graph a w -> List (Tuple a w)
adjacent' vertex graph = maybe Nil M.toList (M.lookup vertex (unwrap graph))

-- Test whether two vertices are adjacent in a graph.
isAdjacent :: forall a w. (Ord a) => a -> a -> Graph a w -> Boolean
isAdjacent a b graph = maybe false (M.member b) (M.lookup a (unwrap graph))

-- Get the weight of the edge between two vertices.
weight :: forall a w. (Ord a) => a -> a -> Graph a w -> Maybe w
weight from to graph = maybe Nothing (M.lookup to) (M.lookup from (unwrap graph))

-- Get the shortest path between two vertices.
shortestPath :: forall a w. (Ord a, Ord w, Semiring w) => a -> a -> Graph a w -> Maybe (List a)
shortestPath from to = shortestPath' (_ == to) from

-- Get the shortest path from a starting vertex to a vertex that satisifes a
-- predicate function.
shortestPath' :: forall a w. (Ord a, Ord w, Semiring w) => (a -> Boolean) -> a -> Graph a w -> Maybe (List a)
shortestPath' p start g = go (PQ.singleton zero start) S.empty (M.singleton start zero) M.empty
  where
    go :: PQueue w a     -- priority queue of fringe vertices
       -> Set a          -- set of visited nodes
       -> Map a w        -- map from vertices to costs
       -> Map a a        -- map from vertices to adjacent vertices
       -> Maybe (List a) -- shortest path
    go fringe visited labels edges =
      if PQ.isEmpty fringe then Nothing
      else
        let smallest = unsafePartial $ PPQ.head fringe
            cost = fst smallest
            vertex = snd smallest
            fringe' = unsafePartial $ PPQ.tail fringe
            visited' = S.insert vertex visited
            labels' = F.foldl (\a (Tuple v c) -> M.insert v c a) labels successors
            successors = L.filter isSuccessor $ successorsAndCosts vertex cost
            isSuccessor (Tuple v c) = not (S.member v visited') && ((not (M.member v labels)) || c < (lookup' v labels))
        in if p vertex then Just $ findPath vertex edges
           else if S.member vertex visited then go fringe' visited labels edges
           else
             let fringe'' = F.foldl (\a (Tuple v c) -> PQ.insert c v a) fringe' successors
                 edges' = F.foldl (\a (Tuple v _) -> M.insert v vertex a) edges successors
             in go fringe'' visited' labels' edges'

    successorsAndCosts :: a -> w -> List (Tuple a w)
    successorsAndCosts v cost = map (\(Tuple v c) -> Tuple v (cost + c)) (adjacent' v g)

    findPath :: a -> Map a a -> List a
    findPath vertex edges
      | M.member vertex edges =
        let vertex' = lookup' vertex edges
        in (findPath vertex' edges) <> L.singleton vertex
      | otherwise = L.singleton vertex

    lookup' :: forall k v. (Ord k) => k -> Map k v -> v
    lookup' k m = unsafePartial $ fromJust $ M.lookup k m

-- Perform a depth-frist traversal of a graph starting at a given node.
traverse :: forall a w. (Ord a) => a -> Graph a w -> List a
traverse from g
  | elem from g =
    let go Nil path = path
        go (v:vs) path
          | F.elem v path = go vs path
          | otherwise = go (adjacent v g <> vs) (v:path)
    in L.reverse $ go (L.singleton from) Nil
  | otherwise = Nil

-- Get the strongly connected components of a graph.
connectedComponents :: forall a w. (Ord a) => Graph a w -> List (Graph a w)
connectedComponents g =
  let go Nil gs = gs
      go (v:vs) gs =
        let vs' = traverse v g
            as = map (\a -> Tuple a (adjacent' a g)) vs'
        in go (vs \\ vs') (L.snoc gs (fromAdjacencyList as))
  in go (vertices g) Nil

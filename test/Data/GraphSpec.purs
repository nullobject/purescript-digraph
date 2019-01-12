module Test.Data.GraphSpec where

import Prelude

import Data.Array (elem) as A
import Data.List (fromFoldable, (!!))
import Data.Maybe (Maybe(..), fromJust)
import Data.Map (empty, isEmpty) as M
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Graph (Graph, adjacent, connectedComponents, deleteEdge, deleteVertex, elem, empty, filter, fromAdjacencyList, insertEdge, insertVertex, isAdjacent, isEmpty, shortestPath, size, traverse, vertices, weight)

graphSpec :: Spec Unit
graphSpec = describe "Graph" do
  let edges = fromFoldable $
    [ Tuple 'A' (fromFoldable [Tuple 'B' 1, Tuple 'C' 2])
    , Tuple 'B' (fromFoldable [Tuple 'A' 1, Tuple 'D' 3])
    , Tuple 'C' (fromFoldable [Tuple 'A' 2, Tuple 'D' 4])
    , Tuple 'D' (fromFoldable [Tuple 'B' 3, Tuple 'C' 4])
    , Tuple 'E' (fromFoldable [Tuple 'F' 1])
    , Tuple 'F' (fromFoldable [Tuple 'E' 1])
    , Tuple 'G' (fromFoldable [])
    ]

  let graph = fromAdjacencyList edges

  describe "empty" do
    it "returns an empty graph" do
      M.isEmpty (unwrap $ empty :: Graph Int Int) `shouldEqual` true

  describe "isEmpty" do
    it "returns an empty graph" do
      isEmpty (wrap M.empty :: Graph Int Int) `shouldEqual` true

  describe "vertices" do
    it "returns the vertices" do
      vertices graph `shouldEqual` fromFoldable ['A', 'B', 'C', 'D', 'E', 'F', 'G']

  describe "size" do
    it "returns the number of vertices" do
      size graph `shouldEqual` 7

  describe "elem" do
    it "returns true if the graph contains a vertex" do
      elem 'A' graph `shouldEqual` true

    it "returns false if the graph does not contain a vertex" do
      elem 'H' graph `shouldEqual` false

  describe "adjacent" do
    it "returns the adjacent vertices given a vertex in the graph" do
      adjacent 'A' graph `shouldEqual` fromFoldable ['B', 'C']
      adjacent 'B' graph `shouldEqual` fromFoldable ['A', 'D']
      adjacent 'C' graph `shouldEqual` fromFoldable ['A', 'D']
      adjacent 'D' graph `shouldEqual` fromFoldable ['B', 'C']
      adjacent 'E' graph `shouldEqual` fromFoldable ['F']
      adjacent 'F' graph `shouldEqual` fromFoldable ['E']

    it "returns an empty list given a vertex with no edges" do
      adjacent 'G' graph `shouldEqual` fromFoldable []

    it "returns an empty list given a vertex not in the graph" do
      adjacent 'H' graph `shouldEqual` fromFoldable []

  describe "isAdjacent" do
    it "returns true if the vertices are adjacent" do
      isAdjacent 'A' 'B' graph `shouldEqual` true

    it "returns false if the vertices are not adjacent" do
      isAdjacent 'A' 'D' graph `shouldEqual` false

  describe "weight" do
    it "returns the weight of the edge between the vertices" do
      weight 'A' 'B' graph `shouldEqual` Just 1
      weight 'A' 'C' graph `shouldEqual` Just 2
      weight 'B' 'D' graph `shouldEqual` Just 3
      weight 'C' 'D' graph `shouldEqual` Just 4

    it "returns nothing if there is no edge between the vertices" do
      weight 'A' 'D' graph `shouldEqual` Nothing

  describe "shortestPath" do
    it "returns the shortest path between the vertices" do
      shortestPath 'A' 'A' graph `shouldEqual` Just (fromFoldable ['A'])
      shortestPath 'A' 'B' graph `shouldEqual` Just (fromFoldable ['A', 'B'])
      shortestPath 'A' 'C' graph `shouldEqual` Just (fromFoldable ['A', 'C'])
      shortestPath 'A' 'D' graph `shouldEqual` Just (fromFoldable ['A', 'B', 'D'])
      shortestPath 'D' 'A' graph `shouldEqual` Just (fromFoldable ['D', 'B', 'A'])
      shortestPath 'D' 'B' graph `shouldEqual` Just (fromFoldable ['D', 'B'])
      shortestPath 'D' 'C' graph `shouldEqual` Just (fromFoldable ['D', 'C'])
      shortestPath 'D' 'D' graph `shouldEqual` Just (fromFoldable ['D'])

    it "returns nothing if there is no shortest path between the vertices" do
      shortestPath 'A' 'E' graph `shouldEqual` Nothing

  describe "traverse" do
    it "returns the vertices visited in a depth-first traversal of the graph" do
      traverse 'A' graph `shouldEqual` fromFoldable ['A', 'B', 'D', 'C']
      traverse 'B' graph `shouldEqual` fromFoldable ['B', 'A', 'C', 'D']
      traverse 'C' graph `shouldEqual` fromFoldable ['C', 'A', 'B', 'D']
      traverse 'D' graph `shouldEqual` fromFoldable ['D', 'B', 'A', 'C']
      traverse 'E' graph `shouldEqual` fromFoldable ['E', 'F']
      traverse 'F' graph `shouldEqual` fromFoldable ['F', 'E']
      traverse 'G' graph `shouldEqual` fromFoldable ['G']

    it "returns an empty list given a vertex not in the graph" do
      traverse 'H' graph `shouldEqual` fromFoldable []

  describe "connectedComponents" do
    it "returns the connected components of the graph" do
      let components = connectedComponents graph
      vertices (unsafePartial $ fromJust $ components !! 0) `shouldEqual` fromFoldable ['A', 'B', 'C', 'D']
      vertices (unsafePartial $ fromJust $ components !! 1) `shouldEqual` fromFoldable ['E', 'F']
      vertices (unsafePartial $ fromJust $ components !! 2) `shouldEqual` fromFoldable ['G']

  describe "insertVertex" do
    it "inserts a vertex into the graph" do
      vertices (insertVertex 'H' graph) `shouldEqual` fromFoldable ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']

  describe "insertEdge" do
    it "inserts an edge into the graph" do
      let graph' = insertEdge 'F' 'G' 5 graph
      weight 'F' 'G' graph' `shouldEqual` Just 5

  describe "deleteVertex" do
    it "deletes a vertex from the graph" do
      vertices (deleteVertex 'A' graph) `shouldEqual` fromFoldable ['B', 'C', 'D', 'E', 'F', 'G']

    it "returns the graph unchanged if there is no matching vertex" do
      vertices (deleteVertex 'H' graph) `shouldEqual` fromFoldable ['A', 'B', 'C', 'D', 'E', 'F', 'G']

    it "deletes the edges incident on a vertex from the graph" do
      let graph' = deleteVertex 'A' graph
      isAdjacent 'A' 'B' graph' `shouldEqual` false
      isAdjacent 'B' 'A' graph' `shouldEqual` false

  describe "deleteEdge" do
    it "deletes an edge from the graph" do
      let graph' = deleteEdge 'A' 'B' graph
      isAdjacent 'A' 'B' graph' `shouldEqual` false
      isAdjacent 'B' 'A' graph' `shouldEqual` true

    it "returns the graph unchanged if there is no matching edge" do
      let graph' = deleteEdge 'F' 'G' graph
      isAdjacent 'F' 'G' graph' `shouldEqual` false

  describe "filter" do
    it "removes matching vertices from the graph" do
      let f vertex = A.elem vertex ['A', 'B', 'C']
      vertices (filter f graph) `shouldEqual` fromFoldable ['D', 'E', 'F', 'G']

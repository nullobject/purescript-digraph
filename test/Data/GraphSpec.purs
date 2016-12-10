module Test.Data.GraphSpec where

import Prelude

import Data.List (fromFoldable, (!!))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Graph (adjacent, connectedComponents, elem, fromAdjacencyList, isAdjacent, shortestPath, size, traverse, vertices, weight)

graphSpec :: forall r. (Spec r) Unit
graphSpec = describe "Graph" do
  let edges = fromFoldable $
    [ Tuple 1 (fromFoldable [Tuple 2 1, Tuple 3 2])
    , Tuple 2 (fromFoldable [Tuple 1 1, Tuple 4 3])
    , Tuple 3 (fromFoldable [Tuple 1 2, Tuple 4 4])
    , Tuple 4 (fromFoldable [Tuple 2 3, Tuple 3 4])
    , Tuple 5 (fromFoldable [Tuple 6 1])
    , Tuple 6 (fromFoldable [Tuple 5 1])
    , Tuple 7 (fromFoldable [])
    ]

  let graph = fromAdjacencyList edges

  describe "vertices" do
    it "returns the vertices" do
      vertices graph `shouldEqual` fromFoldable [1, 2, 3, 4, 5, 6, 7]

  describe "size" do
    it "returns the number of vertices" do
      size graph `shouldEqual` 7

  describe "elem" do
    it "returns true if the graph contains a vertex" do
      elem 1 graph `shouldEqual` true

    it "returns false if the graph does not contain a vertex" do
      elem 8 graph `shouldEqual` false

  describe "adjacent" do
    it "returns the adjacent vertices given a vertex in the graph" do
      adjacent 1 graph `shouldEqual` fromFoldable [2, 3]
      adjacent 2 graph `shouldEqual` fromFoldable [1, 4]
      adjacent 3 graph `shouldEqual` fromFoldable [1, 4]
      adjacent 4 graph `shouldEqual` fromFoldable [2, 3]
      adjacent 5 graph `shouldEqual` fromFoldable [6]
      adjacent 6 graph `shouldEqual` fromFoldable [5]

    it "returns an empty list given a vertex with no edges" do
      adjacent 7 graph `shouldEqual` fromFoldable []

    it "returns an empty list given a vertex not in the graph" do
      adjacent 8 graph `shouldEqual` fromFoldable []

  describe "isAdjacent" do
    it "returns true if the vertices are adjacent" do
      isAdjacent 1 2 graph `shouldEqual` true

    it "returns false if the vertices are not adjacent" do
      isAdjacent 1 4 graph `shouldEqual` false

  describe "weight" do
    it "returns the weight of the edge between the vertices" do
      weight 1 2 graph `shouldEqual` Just 1
      weight 1 3 graph `shouldEqual` Just 2
      weight 2 4 graph `shouldEqual` Just 3
      weight 3 4 graph `shouldEqual` Just 4

    it "returns nothing if there is no edge between the vertices" do
      weight 1 4 graph `shouldEqual` Nothing

  describe "shortestPath" do
    it "returns the shortest path between the vertices" do
      shortestPath 1 1 graph `shouldEqual` Just (fromFoldable [1])
      shortestPath 1 2 graph `shouldEqual` Just (fromFoldable [1, 2])
      shortestPath 1 3 graph `shouldEqual` Just (fromFoldable [1, 3])
      shortestPath 1 4 graph `shouldEqual` Just (fromFoldable [1, 2, 4])
      shortestPath 4 1 graph `shouldEqual` Just (fromFoldable [4, 2, 1])
      shortestPath 4 2 graph `shouldEqual` Just (fromFoldable [4, 2])
      shortestPath 4 3 graph `shouldEqual` Just (fromFoldable [4, 3])
      shortestPath 4 4 graph `shouldEqual` Just (fromFoldable [4])

    it "returns nothing if there is no shortest path between the vertices" do
      shortestPath 1 5 graph `shouldEqual` Nothing

  describe "traverse" do
    it "returns the vertices visited in a depth-first traversal of the graph" do
      traverse 1 graph `shouldEqual` fromFoldable [1, 2, 4, 3]
      traverse 2 graph `shouldEqual` fromFoldable [2, 1, 3, 4]
      traverse 3 graph `shouldEqual` fromFoldable [3, 1, 2, 4]
      traverse 4 graph `shouldEqual` fromFoldable [4, 2, 1, 3]
      traverse 5 graph `shouldEqual` fromFoldable [5, 6]
      traverse 6 graph `shouldEqual` fromFoldable [6, 5]
      traverse 7 graph `shouldEqual` fromFoldable [7]

    it "returns an empty list given a vertex not in the graph" do
      traverse 8 graph `shouldEqual` fromFoldable []

  describe "connectedComponents" do
    it "returns the connected components of the graph" do
      let components = connectedComponents graph
      vertices (unsafePartial $ fromJust $ components !! 0) `shouldEqual` fromFoldable [1, 2, 3, 4]
      vertices (unsafePartial $ fromJust $ components !! 1) `shouldEqual` fromFoldable [5, 6]
      vertices (unsafePartial $ fromJust $ components !! 2) `shouldEqual` fromFoldable [7]

module Test.Data.GraphSpec where

import Prelude (Unit, bind, ($))

import Data.List (fromFoldable, (!!))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Graph

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

  let g = graph edges

  describe "vertices" do
    it "returns the vertices" do
      vertices g `shouldEqual` fromFoldable [1, 2, 3, 4, 5, 6, 7]

  describe "size" do
    it "returns the number of vertices" do
      size g `shouldEqual` 7

  describe "elem" do
    it "returns true if the graph contains a vertex" do
      elem 1 g `shouldEqual` true

    it "returns false if the graph does not contain a vertex" do
      elem 8 g `shouldEqual` false

  describe "adjacent" do
    it "returns the adjacent vertices given a vertex in the graph" do
      adjacent 1 g `shouldEqual` fromFoldable [2, 3]
      adjacent 2 g `shouldEqual` fromFoldable [1, 4]
      adjacent 3 g `shouldEqual` fromFoldable [1, 4]
      adjacent 4 g `shouldEqual` fromFoldable [2, 3]
      adjacent 5 g `shouldEqual` fromFoldable [6]
      adjacent 6 g `shouldEqual` fromFoldable [5]

    it "returns an empty list given a vertex with no edges" do
      adjacent 7 g `shouldEqual` fromFoldable []

    it "returns an empty list given a vertex not in the graph" do
      adjacent 8 g `shouldEqual` fromFoldable []

  describe "isAdjacent" do
    it "returns true if the vertices are adjacent" do
      isAdjacent 1 2 g `shouldEqual` true

    it "returns false if the vertices are not adjacent" do
      isAdjacent 1 4 g `shouldEqual` false

  describe "weight" do
    it "returns the weight of the edge between the vertices" do
      weight 1 2 g `shouldEqual` Just 1
      weight 1 3 g `shouldEqual` Just 2
      weight 2 4 g `shouldEqual` Just 3
      weight 3 4 g `shouldEqual` Just 4

    it "returns nothing if there is no edge between the vertices" do
      weight 1 4 g `shouldEqual` Nothing

  describe "shortestPath" do
    it "returns the shortest path between the vertices" do
      shortestPath 1 1 g `shouldEqual` Just (fromFoldable [1])
      shortestPath 1 2 g `shouldEqual` Just (fromFoldable [1, 2])
      shortestPath 1 3 g `shouldEqual` Just (fromFoldable [1, 3])
      shortestPath 1 4 g `shouldEqual` Just (fromFoldable [1, 2, 4])
      shortestPath 4 1 g `shouldEqual` Just (fromFoldable [4, 2, 1])
      shortestPath 4 2 g `shouldEqual` Just (fromFoldable [4, 2])
      shortestPath 4 3 g `shouldEqual` Just (fromFoldable [4, 3])
      shortestPath 4 4 g `shouldEqual` Just (fromFoldable [4])

    it "returns nothing if there is no shortest path between the vertices" do
      shortestPath 1 5 g `shouldEqual` Nothing

  describe "traverse" do
    it "returns the vertices visited in a depth-first traversal of the graph" do
      traverse 1 g `shouldEqual` fromFoldable [1, 2, 4, 3]
      traverse 2 g `shouldEqual` fromFoldable [2, 1, 3, 4]
      traverse 3 g `shouldEqual` fromFoldable [3, 1, 2, 4]
      traverse 4 g `shouldEqual` fromFoldable [4, 2, 1, 3]
      traverse 5 g `shouldEqual` fromFoldable [5, 6]
      traverse 6 g `shouldEqual` fromFoldable [6, 5]
      traverse 7 g `shouldEqual` fromFoldable [7]

    it "returns an empty list given a vertex not in the graph" do
      traverse 8 g `shouldEqual` fromFoldable []

  describe "connectedComponents" do
    it "returns the connected components of the graph" do
      let components = connectedComponents g
      vertices (unsafePartial $ fromJust $ components !! 0) `shouldEqual` fromFoldable [1, 2, 3, 4]
      vertices (unsafePartial $ fromJust $ components !! 1) `shouldEqual` fromFoldable [5, 6]
      vertices (unsafePartial $ fromJust $ components !! 2) `shouldEqual` fromFoldable [7]

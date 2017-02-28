# purescript-digraph

[![Build Status](https://travis-ci.org/nullobject/purescript-digraph.svg?branch=master)](https://travis-ci.org/nullobject/purescript-digraph)

A directed graph library for PureScript.

## Examples

![Graph](/images/graph.png)

The directed graph pictured above can be represented with an `AdjacencyList`.
An adjacency list is a list of tuples that contain a vertex and a list of edges
to its adjacent vertices. A `Graph` can be constructed from an `AdjacencyList`.

```haskell
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
```

### Size

The `size` function returns the number of vertices in a graph.

```haskell
size graph
-- 7
```

### Vertices

The `vertices` function returns a list of the vertices in a graph.

```haskell
vertices graph
-- ['A', 'B', 'C', 'D', 'E', 'F', 'G']
```

### Adjacent vertices

The `adjacent` function returns all vertices connected to a vertex by an edge.

```haskell
adjacent 'A' graph
-- ['B', 'C']
```

### Shortest path

The `shortestPath` function calculates the shortest path between two vertices
using [Dijkstra'a
algorithm](https://en.wikipedia.org/wiki/Dijkstra's_algorithm).

```haskell
shortestPath 'A' 'D' graph
-- ['A', 'B', 'D']
```

### Connected components

The `connectedComponents` function calculates a list of connected components in a graph.

A [connected
component](https://en.wikipedia.org/wiki/Connected_component_(graph_theory)) is
a maximal subgraph in which any two vertices are connected to each other by a
path.

```haskell
connectedComponents graph
-- [Graph, Graph, Graph]
```

## API

[Data.Graph](https://pursuit.purescript.org/packages/purescript-digraph)

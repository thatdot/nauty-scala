# nauty-scala

A Scala port of **nauty** - the graph automorphism and canonical labeling library.

## Credits

The nauty algorithm was developed by **Brendan McKay** and **Adolfo Piperno**. This Scala implementation is based on nauty version 2.9.3.

**Original nauty:**
- Website: https://pallini.di.uniroma1.it
- Paper: B. D. McKay and A. Piperno, "Practical Graph Isomorphism, II", Journal of Symbolic Computation, 60 (2014), pp. 94-112.

## License

The original version of nauty is released under the APACHE 2.0 License. So is this version.

## A.I. Disclaimer

This library was created using A.I. tools. In particular, Claude Code (4.5/4.6) was provided with the original C implemention (linked above) and instructed to create a faithful translation into Scala. It has been deliberately tested with human-guided and A.I.-generated tests you can find in the test suite.

## Overview

This library provides:
- **Graph automorphism group computation** - Find generators of the automorphism group
- **Canonical labeling** - Compute a canonical form for graph isomorphism testing
- **Graph I/O** - Read and write Graph6, Sparse6, and Digraph6 formats

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "com.thatdot" %% "nauty-scala" % "0.9.0"
```

Or build from source:

```bash
sbt compile
sbt test
sbt publishLocal
```

## Quick Start

To try these examples interactively, start a REPL session with `sbt console`.

### Graph Construction

```scala
import com.thatdot.nauty._
import com.thatdot.nauty.graph._
import com.thatdot.nauty.core._
import com.thatdot.nauty.group._

// From edge list
val square = DenseGraph.fromEdges(4, Seq((0,1), (1,2), (2,3), (3,0)))

// Common graphs
val complete = DenseGraph.complete(6)    // K6
val cycle = DenseGraph.cycle(8)          // C8
val path = DenseGraph.path(5)            // P5

// Directed graph
val digraph = DenseGraph.fromEdges(3, Seq((0,1), (1,2), (2,0)), directed = true)
```

### Computing Automorphisms

```scala
// The square (cycle of 4) has 8 automorphisms (dihedral group D4)
val square = DenseGraph.cycle(4)
val result = Nauty.densenauty(square)

result.groupSize          // 8
result.numOrbits          // 1 (all vertices equivalent)
result.generators.size    // 2 generators suffice for D4

// View generators in cycle notation
result.generators.map(_.toCycleString)
// e.g., List("(0 1 2 3)", "(0 3)(1 2)")

// The first generator rotates, the second reflects
val rotate = result.generators(0)
val reflect = result.generators(1)

rotate.order   // 4 (rotate 4 times = identity)
reflect.order  // 2 (reflect twice = identity)

// Generate the full group (all 8 elements)
val fullGroup = Nauty.generateGroup(result.generators)
fullGroup.size  // 8
fullGroup.map(_.toCycleString).toSeq.sorted
// List("()", "(0 1)(2 3)", "(0 1 2 3)", "(0 2)", "(0 2)(1 3)",
//      "(0 3)(1 2)", "(0 3 2 1)", "(1 3)")
```

### The Petersen Graph

The Petersen graph is a famous 3-regular graph on 10 vertices with automorphism group of order 120.

```scala
// Build Petersen graph: outer pentagon + inner pentagram + spokes
val edges = Seq(
  // Outer pentagon
  (0,1), (1,2), (2,3), (3,4), (4,0),
  // Inner pentagram (star)
  (5,7), (7,9), (9,6), (6,8), (8,5),
  // Spokes connecting outer to inner
  (0,5), (1,6), (2,7), (3,8), (4,9)
)
val petersen = DenseGraph.fromEdges(10, edges)

val result = Nauty.densenauty(petersen)
result.groupSize   // 120 (isomorphic to S5, the symmetric group on 5 elements)
result.numOrbits   // 1 (vertex-transitive: all vertices are equivalent)
```

### Graph Isomorphism

```scala
// Two different labelings of the same graph
val g1 = DenseGraph.fromEdges(4, Seq((0,1), (1,2), (2,3), (3,0)))  // Square 0-1-2-3
val g2 = DenseGraph.fromEdges(4, Seq((0,2), (2,1), (1,3), (3,0)))  // Square 0-2-1-3

Nauty.isIsomorphic(g1, g2)  // true

// A different graph (path, not cycle)
val g3 = DenseGraph.fromEdges(4, Seq((0,1), (1,2), (2,3)))

Nauty.isIsomorphic(g1, g3)  // false
```

### Canonical Forms and Hashing

```scala
// Canonical form produces a unique representative for each isomorphism class
val g1 = DenseGraph.fromEdges(4, Seq((0,1), (1,2), (2,3), (3,0)))
val g2 = DenseGraph.fromEdges(4, Seq((0,2), (2,1), (1,3), (3,0)))

val c1 = Nauty.canonicalForm(g1)
val c2 = Nauty.canonicalForm(g2)

c1 == c2  // true - same canonical form

// Canonical hash for indexing/deduplication
val opts = NautyOptions.defaultGraph.withCanon
val r1 = Nauty.densenauty(g1, opts)
val r2 = Nauty.densenauty(g2, opts)

r1.canonicalHash == r2.canonicalHash  // true
```

### Vertex Coloring (Partitions)

Use partitions to restrict automorphisms to those preserving vertex colors.

```scala
// Triangle: normally has 6 automorphisms (S3)
val triangle = DenseGraph.fromEdges(3, Seq((0,1), (1,2), (2,0)))
Nauty.densenauty(triangle).groupSize  // 6

// Color vertex 0 differently - now only 2 automorphisms (swap 1 and 2)
val opts = NautyOptions.defaultGraph
  .withPartition(Seq(Seq(0), Seq(1, 2)))  // vertex 0 alone, vertices 1,2 together

val result = Nauty.densenauty(triangle, opts)
result.groupSize  // 2
result.generators.map(_.toCycleString)  // List("(1 2)")
```

### Working with Permutations

```scala
// Create permutations
val p1 = Permutation(1, 2, 0, 3)             // From images: 0→1, 1→2, 2→0, 3→3
val p2 = Permutation.cyclic(4)               // (0 1 2 3)
val p3 = Permutation.transposition(4, 0, 1)  // Swap 0 and 1
val p4 = Permutation.fromCycles(5, List(0, 1, 2), List(3, 4))  // (0 1 2)(3 4)

// Cycle notation
p1.toCycleString  // "(0 1 2)"
p4.toCycleString  // "(0 1 2)(3 4)"

// Properties
p1.order          // 3 (p1^3 = identity)
p4.order          // 6 (lcm of cycle lengths 3 and 2)
p1.movedPoints    // Set(0, 1, 2)
p1.fixedPoints    // Set(3)

// Composition: (p1 * p3)(x) = p1(p3(x))
val composed = p1 * p3

// Inverse
val inv = p1.inverse
(p1 * inv).isIdentity  // true

// Powers
p2.pow(2).toCycleString  // "(0 2)(1 3)" - rotate twice
p2.pow(4).isIdentity     // true
```

### Directed Graphs

```scala
// Directed cycle: 0→1→2→0
val dirCycle = DenseGraph.fromEdges(3, Seq((0,1), (1,2), (2,0)), directed = true)
val result = Nauty.densenauty(dirCycle, NautyOptions.defaultDigraph)

result.groupSize  // 3 (can rotate but not reflect)
result.generators.map(_.toCycleString)  // List("(0 1 2)")

// Compare with undirected: triangle has 6 automorphisms
val undirTriangle = DenseGraph.fromEdges(3, Seq((0,1), (1,2), (2,0)))
Nauty.densenauty(undirTriangle).groupSize  // 6
```

### Schreier-Sims Algorithm

Compute exact group order and test membership using Schreier-Sims.

```scala
val square = DenseGraph.cycle(4)
val result = Nauty.densenauty(square)
val generators = result.generators

// Accurate group order
val order = SchreierSims.groupOrder(generators, 4)  // 8

// Test membership: is (0 2)(1 3) in the automorphism group?
val p = Permutation.fromCycles(4, List(0, 2), List(1, 3))
SchreierSims.isMember(p, generators, 4)  // true

// Is (0 1 2) in the group? No - it would map the square to a non-square
val bad = Permutation.fromCycles(4, List(0, 1, 2))
SchreierSims.isMember(bad, generators, 4)  // false

// Compute the full BSGS (Base and Strong Generating Set)
val bsgs = SchreierSims.computeBSGS(generators, 4)
bsgs.order                  // 8
bsgs.base                   // Base points, e.g., Seq(0, 1)
bsgs.strongGenerators.size  // Strong generators
```

### Graph I/O (Graph6/Sparse6/Digraph6)

```scala
import com.thatdot.nauty.io._

// Encode graphs to standard formats
val g6 = Graph6.encode(petersen)   // Compact ASCII encoding
val s6 = Sparse6.encode(petersen)  // More efficient for sparse graphs

// Decode
val decoded = Graph6.decode(g6)
Nauty.isIsomorphic(petersen, decoded)  // true

// Digraph6 for directed graphs
val dirGraph = DenseGraph.fromEdges(3, Seq((0,1), (1,2)), directed = true)
val d6 = Digraph6.encode(dirGraph)
val decodedDir = Digraph6.decode(d6)

// Auto-detect format
val g = GraphIO.decode(encodedString)
```

### Sparse Graphs

For large sparse graphs, use native sparse algorithms for efficiency.

```scala
import com.thatdot.nauty.graph.SparseGraph

// Create sparse graph
val edges = (0 until 1000).map(i => (i, (i + 1) % 1000))  // Cycle of 1000
val sg = SparseGraph.fromEdges(1000, edges)

// Native sparse nauty - no conversion to dense
val result = Nauty.sparsenauty(sg)
result.groupSize  // 2000 (dihedral group D1000)

// Convert between representations when needed
val dense = sg.toDense
val sparse = dense.toSparse
```

### Graph Permutation

```scala
// Apply a permutation to relabel a graph
// Result has edge (p(v), p(w)) iff original has edge (v, w)
val perm = Array(2, 0, 1, 3)  // vertex 0→2, 1→0, 2→1, 3→3
val relabeled = graph.permute(perm)
```

## Package Structure

```
com.thatdot.nauty/
├── bits/           # SetWord, BitOps utilities
├── core/           # Nauty algorithm, Partition, Refinement
├── graph/          # DenseGraph, SparseGraph
├── group/          # Permutation, Orbits, SchreierSims
├── io/             # Graph6, Sparse6, Digraph6 codecs
└── util/           # NautyOptions, NautyStats
```

## Implementation Status

This is a **partial port** of nauty. The following features are implemented:

### Implemented

| Feature | Status | Notes |
|---------|--------|-------|
| Bit operations (`SetWord`) | ✅ Complete | POPCOUNT, FIRSTBIT, set operations |
| Dense graph representation | ✅ Complete | Bit-packed adjacency matrix |
| Sparse graph representation | ✅ Complete | CSR format with native sparse algorithms |
| Partition operations | ✅ Complete | lab/ptn encoding, cell operations, snapshots |
| Permutation operations | ✅ Complete | Composition, inverse, cycles, order |
| Partition refinement | ✅ Complete | Neighbor counting, bucket sort splitting |
| Search tree traversal | ✅ Complete | Explicit stack, backtracking |
| Automorphism detection | ✅ Complete | Basic `isautom` check |
| Canonical labeling | ✅ Complete | `testcanlab`, `updatecan` |
| Orbit computation | ✅ Complete | Union-find based |
| Graph6 I/O | ✅ Complete | Encode/decode |
| Sparse6 I/O | ✅ Complete | Encode/decode |
| Digraph6 I/O | ✅ Complete | Encode/decode |
| Initial partition support | ✅ Complete | Vertex coloring |
| Schreier-Sims | ✅ Complete | BSGS computation, group order, membership testing (pruneset not implemented) |
| Sparse-specific algorithms | ✅ Complete | Native sparse refinement, automorphism testing, `sparsenauty()` |

### Not Yet Implemented

| Feature | Priority | Complexity |
|---------|----------|------------|
| **Traces algorithm** | Low | High |
| - Candidate lists and search trie | | ~10,500 lines in C |
| - Traces-specific refinement | | |
| - Experimental path handling | | |
| **Vertex invariants** | Low | Medium |
| - Adjacency-based invariants | | |
| - Distance-based invariants | | |
| **Advanced pruning** | Medium | Medium |
| - Short prune optimization | | |
| - Long prune optimization | | |
| **Performance tuning** | Medium | Varies |
| - JMH benchmarks | | |
| - Optimization passes | | |

### Known Limitations

1. **Traces**: The Traces algorithm (often faster than nauty for large graphs) is not implemented.

2. **Schreier-Sims pruneset**: The `pruneset()` optimization from C nauty (which prunes the search space using group information during the search) is not implemented. This does not affect correctness—all automorphisms are still found—but may result in exploring more search tree nodes than necessary for some graphs.

3. **Performance**: Expect 2-5x slower than C nauty due to JVM overhead. Sufficient for most applications but not for processing very large graphs.

## Comparison with C nauty

| Aspect | C nauty | nauty-scala |
|--------|---------|-------------|
| API style | Mutable, pointer-based | Immutable results, functional API |
| Type safety | Manual | Strongly typed options and results |
| Platform | Native | JVM (any platform) |
| Performance | Baseline | ~2-5x slower |
| Traces algorithm | ✅ | ❌ Not implemented |
| Schreier-Sims | ✅ | ✅ Complete |

## Running Tests

```bash
sbt test
```

## Contributing

Contributions are welcome, especially for:
- Traces algorithm
- Schreier-Sims pruneset optimization
- Performance improvements
- Additional test cases

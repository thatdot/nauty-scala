# nauty-scala

A partial Scala port of **nauty** (No AUTomorphisms, Yes?) - the graph automorphism and canonical labeling library.

## Credits

The nauty algorithm was developed by **Brendan McKay** and **Adolfo Piperno**. This Scala implementation is based on nauty version 2.9.3.

**Original nauty:**
- Website: https://pallini.di.uniroma1.it
- Paper: B. D. McKay and A. Piperno, "Practical Graph Isomorphism, II", Journal of Symbolic Computation, 60 (2014), pp. 94-112.

This Scala port is distributed under the same license as nauty. See the COPYRIGHT file in the nauty distribution.

## Overview

This library provides:
- **Graph automorphism group computation** - Find generators of the automorphism group
- **Canonical labeling** - Compute a canonical form for graph isomorphism testing
- **Graph I/O** - Read and write Graph6, Sparse6, and Digraph6 formats
- **Cypher pattern support** - Canonicalize and find automorphisms of Cypher graph patterns

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "com.thatdot" %% "nauty-scala" % "0.1.0-SNAPSHOT"
```

Or build from source:

```bash
sbt compile
sbt test
sbt publishLocal
```

## Quick Start

### Basic Graph Operations

```scala
import com.thatdot.nauty._
import com.thatdot.nauty.graph._
import com.thatdot.nauty.core._

// Create a graph
val g = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3), (3, 0)))

// Compute automorphism group
val result = Nauty.densenauty(g)
println(s"Group size: ${result.groupSize}")
println(s"Number of orbits: ${result.numOrbits}")
println(s"Generators: ${result.generators.map(_.toCycleString)}")

// Test isomorphism
val g2 = DenseGraph.fromEdges(4, Seq((0, 2), (2, 1), (1, 3), (3, 0)))
val isIso = Nauty.isIsomorphic(g, g2)  // true

// Compute canonical form
val opts = NautyOptions.defaultGraph.withCanon
val r = Nauty.densenauty(g, opts)
val canonicalGraph = r.canonicalGraph.get
```

### Cypher Pattern Canonicalization

```scala
import com.thatdot.nauty.cypher.CypherPattern

// Canonical hash (for pattern indexing/deduplication)
val hash = CypherPattern.canonicalHash("(a:Person)-[:KNOWS]->(b:Person)")

// Check if two patterns are isomorphic
val isIso = CypherPattern.areIsomorphic(
  "(a:Person)-[:KNOWS]->(b:Person)",
  "(x:Person)-[:KNOWS]->(y:Person)"  // true - same structure
)

// Get automorphism group size
val groupSize = CypherPattern.automorphismGroupSize("(a)-[:X]->(b)<-[:X]-(c)")  // 2

// Get automorphism generators
val generators = CypherPattern.automorphismGenerators("(a)-[:X]->(b)<-[:X]-(c)")

// Generate all automorphic variants (variable swapping)
val variants = CypherPattern.automorphicPatterns("(a)-[:X]->(b)<-[:X]-(c)")
// List((a)-[:X]->(b)<-[:X]-(c), (c)-[:X]->(b)<-[:X]-(a))

// Include syntactic variants (edge direction notation)
val allVariants = CypherPattern.automorphicPatterns(
  "(a:Person)-[:KNOWS]->(b:Person)",
  includeSyntacticVariants = true
)
// List((a:Person)-[:KNOWS]->(b:Person), (b:Person)<-[:KNOWS]-(a:Person))
```

### Graph Construction

```scala
// Empty graph
val empty = DenseGraph.empty(10)

// From edge list
val g = DenseGraph.fromEdges(5, Seq((0,1), (1,2), (2,3), (3,4), (4,0)))

// Common graphs
val complete = DenseGraph.complete(6)    // K6
val cycle = DenseGraph.cycle(8)          // C8
val path = DenseGraph.path(5)            // P5

// Directed graph
val digraph = DenseGraph.fromEdges(3, Seq((0,1), (1,2), (2,0)), directed = true)
```

### Graph I/O

```scala
import com.thatdot.nauty.io._

// Encode to Graph6
val encoded = Graph6.encode(g)

// Decode from Graph6
val decoded = Graph6.decode("D~{")

// Sparse6 for sparse graphs
val sparse6 = Sparse6.encode(g)

// Digraph6 for directed graphs
val digraph6 = Digraph6.encode(digraph)

// Auto-detect format
val g = GraphIO.decode(someString)
```

### Vertex Coloring (Partitions)

```scala
// Use initial partition to restrict automorphisms to color-preserving ones
val opts = NautyOptions.defaultGraph
  .withCanon
  .withPartition(Seq(Seq(0, 1), Seq(2, 3)))  // Vertices 0,1 have one color; 2,3 another

val result = Nauty.densenauty(graph, opts)
```

## Package Structure

```
com.thatdot.nauty/
├── bits/           # SetWord, BitOps utilities
├── core/           # Nauty algorithm, Partition, Refinement
├── cypher/         # Cypher pattern parsing and canonicalization # For demonstration only
├── graph/          # DenseGraph, SparseGraph
├── group/          # Permutation, Orbits
├── io/             # Graph6, Sparse6, Digraph6 codecs
└── util/           # NautyOptions, NautyStats
```

## Implementation Status

This is a **partial port** of nauty. The following features are implemented:

### Implemented (Phases 1-4, 6)

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
| Cypher pattern support | ✅ Complete | Parse, canonicalize, automorphisms |
| Schreier-Sims | ✅ Complete | BSGS computation, group order, membership testing (pruneset not implemented) |
| Sparse-specific algorithms | ✅ Complete | Native sparse refinement, automorphism testing, `sparsenauty()` |

### Not Yet Implemented (Phases 5, 7)

| Feature | Phase | Priority | Complexity |
|---------|-------|----------|------------|
| **Traces algorithm** | 5 | Low | High |
| - Candidate lists and search trie | | | ~10,500 lines in C |
| - Traces-specific refinement | | | |
| - Experimental path handling | | | |
| **Vertex invariants** | - | Low | Medium |
| - Adjacency-based invariants | | | |
| - Distance-based invariants | | | |
| **Advanced pruning** | - | Medium | Medium |
| - Short prune optimization | | | |
| - Long prune optimization | | | |
| **Performance tuning** | 7 | Medium | Varies |
| - JMH benchmarks | | | |
| - Optimization passes | | | |

### Known Limitations

1. **Traces**: The Traces algorithm (often faster than nauty for large graphs) is not implemented.

2. **Schreier-Sims pruneset**: The `pruneset()` optimization from C nauty (which prunes the search space using group information during the search) is not implemented. This does not affect correctness—all automorphisms are still found—but may result in exploring more search tree nodes than necessary for some graphs.

3. **Performance**: Expect 2-5x slower than C nauty due to JVM overhead. Sufficient for most applications but not for processing millions of graphs.

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

Current test coverage: 173 tests across 10 test suites.

## Contributing

Contributions are welcome, especially for:
- Traces algorithm (Phase 5)
- Schreier-Sims pruneset optimization
- Performance improvements
- Additional test cases

## License

This Scala port is distributed under the same license as nauty. See the COPYRIGHT file in the nauty distribution.

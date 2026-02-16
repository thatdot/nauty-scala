package com.thatdot

/**
 * Nauty: Graph Automorphism and Canonical Labeling
 *
 * This is a Scala port of nauty (No AUTomorphisms, Yes?) by Brendan McKay.
 *
 * == Quick Start ==
 *
 * {{{
 * import com.thatdot.nauty._
 * import com.thatdot.nauty.graph._
 * import com.thatdot.nauty.core._
 *
 * // Create a graph
 * val g = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3), (3, 0)))
 *
 * // Compute automorphism group
 * val result = Nauty.densenauty(g)
 * println(s"Group size: \${result.groupSize}")
 * println(s"Number of orbits: \${result.numOrbits}")
 *
 * // Test isomorphism
 * val g2 = DenseGraph.fromEdges(4, Seq((0, 2), (2, 1), (1, 3), (3, 0)))
 * val isIso = Nauty.isIsomorphic(g, g2)
 *
 * // Compute canonical form
 * val canon = Nauty.canonicalForm(g)
 * }}}
 *
 * == Graph Representations ==
 *
 * - [[graph.DenseGraph]]: Bit-packed adjacency matrix, efficient for dense graphs
 * - [[graph.SparseGraph]]: Compressed sparse row format, efficient for sparse graphs
 *
 * == Graph I/O ==
 *
 * - [[io.Graph6]]: Graph6 format (compact ASCII for undirected graphs)
 * - [[io.Sparse6]]: Sparse6 format (efficient for sparse graphs)
 * - [[io.Digraph6]]: Digraph6 format (for directed graphs)
 *
 * == Main Entry Points ==
 *
 * - [[core.Nauty.densenauty]]: Main algorithm for dense graphs
 * - [[core.Nauty.sparsenauty]]: Main algorithm for sparse graphs
 * - [[core.Nauty.isIsomorphic]]: Test graph isomorphism
 * - [[core.Nauty.canonicalForm]]: Compute canonical labeling
 */
package object nauty {
  /** Version string */
  val Version: String = "0.9.0"

  /** Maximum supported number of vertices */
  val MaxN: Int = Int.MaxValue - 2

  // Re-export main types for convenience
  type DenseGraph = graph.DenseGraph
  type SparseGraph = graph.SparseGraph
  type Permutation = group.Permutation
  type NautyResult = core.NautyResult
  type NautyOptions = util.NautyOptions
  type NautyStats = util.NautyStats

  // Re-export companion objects
  val DenseGraph = graph.DenseGraph
  val SparseGraph = graph.SparseGraph
  val Permutation = group.Permutation
  val Nauty = core.Nauty
  val NautyOptions = util.NautyOptions

  // Re-export I/O
  val Graph6 = io.Graph6
  val Sparse6 = io.Sparse6
  val Digraph6 = io.Digraph6
  val GraphIO = io.GraphIO
}

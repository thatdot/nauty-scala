package com.thatdot.nauty.graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Tests for graph property computations: girth, diameter, radius, connectivity.
 * These match the graph property tests available in C nauty's testg tool.
 */
class GraphPropertiesSpec extends AnyFlatSpec with Matchers {

  //
  // GIRTH TESTS
  //

  "Girth" should "be 3 for complete graphs K_n (n >= 3)" in {
    for (n <- 3 to 8) {
      val g = SparseGraph.complete(n)
      withClue(s"K$n girth: ") {
        g.girth shouldBe 3
      }
    }
  }

  it should "be n for cycle graphs C_n" in {
    for (n <- 3 to 10) {
      val g = SparseGraph.cycle(n)
      withClue(s"C$n girth: ") {
        g.girth shouldBe n
      }
    }
  }

  it should "be infinity for path graphs (acyclic)" in {
    for (n <- 2 to 8) {
      val g = SparseGraph.path(n)
      withClue(s"P$n girth: ") {
        g.girth shouldBe Int.MaxValue
      }
    }
  }

  it should "be infinity for star graphs (acyclic)" in {
    for (n <- 3 to 8) {
      val edges = (1 until n).map(i => (0, i))
      val g = SparseGraph.fromEdges(n, edges)
      withClue(s"S$n girth: ") {
        g.girth shouldBe Int.MaxValue
      }
    }
  }

  it should "be 5 for the Petersen graph" in {
    val petersen = SparseGraph.fromEdges(10, Seq(
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))
    petersen.girth shouldBe 5
  }

  it should "be 4 for complete bipartite K_{m,n} (m,n >= 2)" in {
    val testCases = Seq((2, 2), (2, 3), (3, 3), (2, 4), (3, 4))
    for ((m, n) <- testCases) {
      val edges = for (i <- 0 until m; j <- m until m + n) yield (i, j)
      val g = SparseGraph.fromEdges(m + n, edges)
      withClue(s"K_{$m,$n} girth: ") {
        g.girth shouldBe 4
      }
    }
  }

  it should "be 4 for the cube graph (Q_3)" in {
    val q3Edges = Seq(
      (0, 1), (1, 3), (3, 2), (2, 0),
      (4, 5), (5, 7), (7, 6), (6, 4),
      (0, 4), (1, 5), (2, 6), (3, 7)
    )
    val q3 = SparseGraph.fromEdges(8, q3Edges)
    q3.girth shouldBe 4
  }

  it should "be 6 for the Heawood graph" in {
    // Heawood graph is the Levi graph of the Fano plane (NOT GP(7,2))
    // 14 vertices: 7 points (0-6) and 7 lines (7-13)
    // Edges connect point i to line j iff point i is on line j in Fano plane
    // Fano plane lines: {0,1,3}, {1,2,4}, {2,3,5}, {3,4,6}, {4,5,0}, {5,6,1}, {6,0,2}
    val heawoodEdges = Seq(
      // Line 7 = {0,1,3}
      (0, 7), (1, 7), (3, 7),
      // Line 8 = {1,2,4}
      (1, 8), (2, 8), (4, 8),
      // Line 9 = {2,3,5}
      (2, 9), (3, 9), (5, 9),
      // Line 10 = {3,4,6}
      (3, 10), (4, 10), (6, 10),
      // Line 11 = {4,5,0}
      (4, 11), (5, 11), (0, 11),
      // Line 12 = {5,6,1}
      (5, 12), (6, 12), (1, 12),
      // Line 13 = {6,0,2}
      (6, 13), (0, 13), (2, 13)
    )
    val heawood = SparseGraph.fromEdges(14, heawoodEdges)
    heawood.girth shouldBe 6
  }

  //
  // DIAMETER TESTS
  //

  "Diameter" should "be 1 for complete graphs K_n (n >= 2)" in {
    for (n <- 2 to 8) {
      val g = SparseGraph.complete(n)
      withClue(s"K$n diameter: ") {
        g.diameter shouldBe 1
      }
    }
  }

  it should "be floor(n/2) for cycle graphs C_n" in {
    for (n <- 3 to 12) {
      val g = SparseGraph.cycle(n)
      withClue(s"C$n diameter: ") {
        g.diameter shouldBe n / 2
      }
    }
  }

  it should "be n-1 for path graphs P_n" in {
    for (n <- 2 to 10) {
      val g = SparseGraph.path(n)
      withClue(s"P$n diameter: ") {
        g.diameter shouldBe n - 1
      }
    }
  }

  it should "be 2 for star graphs S_n (n >= 3)" in {
    for (n <- 3 to 10) {
      val edges = (1 until n).map(i => (0, i))
      val g = SparseGraph.fromEdges(n, edges)
      withClue(s"S$n diameter: ") {
        g.diameter shouldBe 2
      }
    }
  }

  it should "be 2 for the Petersen graph" in {
    val petersen = SparseGraph.fromEdges(10, Seq(
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))
    petersen.diameter shouldBe 2
  }

  it should "be 3 for the cube graph (Q_3)" in {
    val q3Edges = Seq(
      (0, 1), (1, 3), (3, 2), (2, 0),
      (4, 5), (5, 7), (7, 6), (6, 4),
      (0, 4), (1, 5), (2, 6), (3, 7)
    )
    val q3 = SparseGraph.fromEdges(8, q3Edges)
    q3.diameter shouldBe 3
  }

  it should "be infinity for disconnected graphs" in {
    // Two K3 components
    val edges = Seq((0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3))
    val g = SparseGraph.fromEdges(6, edges)
    g.diameter shouldBe Int.MaxValue
  }

  //
  // RADIUS TESTS
  //

  "Radius" should "be 1 for complete graphs K_n (n >= 2)" in {
    for (n <- 2 to 8) {
      val g = SparseGraph.complete(n)
      withClue(s"K$n radius: ") {
        g.radius shouldBe 1
      }
    }
  }

  it should "be ceil((n-1)/2) for path graphs P_n" in {
    for (n <- 2 to 10) {
      val g = SparseGraph.path(n)
      val expected = (n - 1 + 1) / 2  // ceil((n-1)/2)
      withClue(s"P$n radius: ") {
        g.radius shouldBe expected
      }
    }
  }

  it should "be floor(n/2) for cycle graphs C_n" in {
    for (n <- 3 to 12) {
      val g = SparseGraph.cycle(n)
      withClue(s"C$n radius: ") {
        g.radius shouldBe n / 2
      }
    }
  }

  it should "be 1 for star graphs S_n (n >= 3)" in {
    for (n <- 3 to 10) {
      val edges = (1 until n).map(i => (0, i))
      val g = SparseGraph.fromEdges(n, edges)
      withClue(s"S$n radius: ") {
        g.radius shouldBe 1
      }
    }
  }

  //
  // CONNECTIVITY TESTS
  //

  "isConnected" should "be true for complete graphs" in {
    for (n <- 1 to 8) {
      val g = SparseGraph.complete(n)
      g.isConnected shouldBe true
    }
  }

  it should "be true for cycles" in {
    for (n <- 3 to 10) {
      val g = SparseGraph.cycle(n)
      g.isConnected shouldBe true
    }
  }

  it should "be true for paths" in {
    for (n <- 1 to 10) {
      val g = SparseGraph.path(n)
      g.isConnected shouldBe true
    }
  }

  it should "be false for disconnected graphs" in {
    // Two K3 components
    val edges = Seq((0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3))
    val g = SparseGraph.fromEdges(6, edges)
    g.isConnected shouldBe false
  }

  it should "be false for graphs with isolated vertices" in {
    // Triangle + isolated vertex
    val edges = Seq((0, 1), (1, 2), (2, 0))
    val g = SparseGraph.fromEdges(4, edges)
    g.isConnected shouldBe false
  }

  it should "be true for single vertex" in {
    val g = SparseGraph.fromEdges(1, Seq.empty)
    g.isConnected shouldBe true
  }

  it should "be true for empty graph (0 vertices)" in {
    val g = SparseGraph.fromEdges(0, Seq.empty)
    g.isConnected shouldBe true
  }

  //
  // DENSE GRAPH COMPATIBILITY
  //

  "DenseGraph properties" should "match SparseGraph properties" in {
    val testGraphs = Seq(
      ("K5", SparseGraph.complete(5)),
      ("C7", SparseGraph.cycle(7)),
      ("P5", SparseGraph.path(5)),
      ("Petersen", SparseGraph.fromEdges(10, Seq(
        (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
        (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
        (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
      )))
    )

    for ((name, sparse) <- testGraphs) {
      val dense = sparse.toDense
      withClue(s"$name girth: ") {
        dense.girth shouldBe sparse.girth
      }
      withClue(s"$name diameter: ") {
        dense.diameter shouldBe sparse.diameter
      }
      withClue(s"$name radius: ") {
        dense.radius shouldBe sparse.radius
      }
      withClue(s"$name isConnected: ") {
        dense.isConnected shouldBe sparse.isConnected
      }
    }
  }
}

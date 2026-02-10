package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.{SparseGraph, DenseGraph}
import com.thatdot.nauty.util.NautyOptions
import com.thatdot.nauty.group.Permutation
import scala.util.Random

/**
 * Comprehensive stress tests for sparse nauty implementation.
 * These tests verify correctness by:
 * 1. Comparing with dense nauty results
 * 2. Testing canonical form consistency
 * 3. Verifying automorphism generators
 * 4. Testing isomorphism detection
 */
class SparseNautyStressSpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * factorial(n - 1)
  private def doubleFactorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * doubleFactorial(n - 2)

  private def isValidAutomorphism(g: SparseGraph, perm: Permutation): Boolean = {
    val n = g.n
    for (i <- 0 until n; j <- g.neighbors(i)) {
      if (!g.hasEdge(perm(i), perm(j))) return false
    }
    true
  }

  // Helper to create a random permutation of a graph
  private def permuteGraph(g: SparseGraph, seed: Long): (SparseGraph, Array[Int]) = {
    val rng = new Random(seed)
    val perm = rng.shuffle((0 until g.n).toList).toArray
    (g.permute(perm), perm)
  }

  //
  // COMPLETE GRAPHS - Known group sizes
  //

  "Complete graphs K_n" should "have automorphism group S_n" in {
    for (n <- 2 to 8) {
      val g = SparseGraph.complete(n)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

      withClue(s"K$n group size: ") {
        result.groupSize shouldBe BigDecimal(factorial(n))
      }

      // All generators should be valid automorphisms
      for (gen <- result.generators) {
        withClue(s"K$n generator ${gen.toCycleString}: ") {
          isValidAutomorphism(g, gen) shouldBe true
        }
      }
    }
  }

  //
  // CYCLE GRAPHS - Known group sizes (dihedral)
  //

  "Cycle graphs C_n" should "have automorphism group D_n of order 2n" in {
    for (n <- 3 to 12) {
      val g = SparseGraph.cycle(n)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

      withClue(s"C$n group size: ") {
        result.groupSize shouldBe BigDecimal(2 * n)
      }

      for (gen <- result.generators) {
        withClue(s"C$n generator ${gen.toCycleString}: ") {
          isValidAutomorphism(g, gen) shouldBe true
        }
      }
    }
  }

  //
  // PATH GRAPHS - Known group sizes (Z_2)
  //

  "Path graphs P_n" should "have automorphism group Z_2 for n >= 2" in {
    for (n <- 2 to 10) {
      val g = SparseGraph.path(n)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

      withClue(s"P$n group size: ") {
        result.groupSize shouldBe BigDecimal(2)
      }
    }
  }

  //
  // STAR GRAPHS - Known group sizes
  //

  "Star graphs S_n" should "have automorphism group S_{n-1}" in {
    for (n <- 3 to 8) {
      // Star with center 0 and leaves 1..n-1
      val edges = (1 until n).map(i => (0, i))
      val g = SparseGraph.fromEdges(n, edges)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

      withClue(s"S$n group size: ") {
        result.groupSize shouldBe BigDecimal(factorial(n - 1))
      }

      // Center should be in its own orbit
      result.numOrbits shouldBe 2
    }
  }

  //
  // COMPLETE BIPARTITE GRAPHS - Known group sizes
  //

  "Complete bipartite K_{m,n}" should "have automorphism group S_m x S_n" in {
    val testCases = Seq((2, 2), (2, 3), (3, 3), (2, 4), (3, 4))

    for ((m, n) <- testCases) {
      val edges = for (i <- 0 until m; j <- m until m + n) yield (i, j)
      val g = SparseGraph.fromEdges(m + n, edges)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

      val expected = if (m == n) factorial(m) * factorial(n) * 2 else factorial(m) * factorial(n)

      withClue(s"K_{$m,$n} group size: ") {
        result.groupSize shouldBe BigDecimal(expected)
      }
    }
  }

  //
  // PETERSEN GRAPH - Known group size 120
  //

  "Petersen graph" should "have automorphism group of order 120" in {
    val petersen = SparseGraph.fromEdges(10, Seq(
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))

    val result = SparseNauty.sparsenauty(petersen, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(120)
  }

  //
  // HYPERCUBE GRAPHS - Known group sizes
  //

  "Hypercube Q_n" should "have automorphism group of order 2^n * n!" in {
    // Q_2 = C_4, already tested
    // Q_3 = cube graph
    val q3Edges = Seq(
      (0, 1), (1, 3), (3, 2), (2, 0),  // Bottom face
      (4, 5), (5, 7), (7, 6), (6, 4),  // Top face
      (0, 4), (1, 5), (2, 6), (3, 7)   // Vertical edges
    )
    val q3 = SparseGraph.fromEdges(8, q3Edges)
    val result = SparseNauty.sparsenauty(q3, NautyOptions.defaultSparseGraph.withSchreier)

    // |Aut(Q_3)| = 2^3 * 3! = 8 * 6 = 48
    withClue("Q_3 group size: ") {
      result.groupSize shouldBe BigDecimal(48)
    }
  }

  //
  // CONSISTENCY TESTS: Sparse vs Dense
  //

  "Sparse and dense nauty" should "give same group sizes for various graphs" in {
    val graphs = Seq(
      ("K5", SparseGraph.complete(5)),
      ("C7", SparseGraph.cycle(7)),
      ("P5", SparseGraph.path(5)),
      ("Star4", SparseGraph.fromEdges(4, Seq((0, 1), (0, 2), (0, 3)))),
      ("K23", SparseGraph.fromEdges(5, Seq((0, 2), (0, 3), (0, 4), (1, 2), (1, 3), (1, 4))))
    )

    for ((name, sparse) <- graphs) {
      val dense = sparse.toDense

      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)

      withClue(s"$name group size (sparse vs dense): ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }

  //
  // CANONICAL FORM CONSISTENCY
  //

  "Canonical form" should "be consistent across permutations of the same graph" in {
    val baseGraphs = Seq(
      ("K4", SparseGraph.complete(4)),
      ("C5", SparseGraph.cycle(5)),
      ("P4", SparseGraph.path(4)),
      ("Petersen", SparseGraph.fromEdges(10, Seq(
        (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
        (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
        (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
      )))
    )

    for ((name, base) <- baseGraphs) {
      val opts = NautyOptions.defaultSparseGraph.withCanon
      val baseResult = SparseNauty.sparsenauty(base, opts)
      val baseCanon = baseResult.canonicalGraph.get

      // Test multiple random permutations
      for (seed <- 1 to 5) {
        val (permuted, perm) = permuteGraph(base, seed)
        val permResult = SparseNauty.sparsenauty(permuted, opts)
        val permCanon = permResult.canonicalGraph.get

        withClue(s"$name permutation $seed: ") {
          baseCanon shouldBe permCanon
        }
      }
    }
  }

  //
  // ISOMORPHISM DETECTION
  //

  "Isomorphism detection" should "correctly identify isomorphic graphs" in {
    val base = SparseGraph.complete(5)

    for (seed <- 1 to 10) {
      val (permuted, _) = permuteGraph(base, seed)

      withClue(s"K5 permutation $seed: ") {
        SparseNauty.isIsomorphic(base, permuted) shouldBe true
      }
    }
  }

  it should "correctly reject non-isomorphic graphs" in {
    val cycle6 = SparseGraph.cycle(6)
    val twoTriangles = SparseGraph.fromEdges(6, Seq(
      (0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3)
    ))

    SparseNauty.isIsomorphic(cycle6, twoTriangles) shouldBe false
  }

  //
  // ORBIT COMPUTATION
  //

  "Orbit computation" should "partition vertices correctly for path graphs" in {
    for (n <- 3 to 8) {
      val g = SparseGraph.path(n)
      val result = SparseNauty.sparsenauty(g)

      // Path has reflection symmetry, so vertex i and n-1-i are in same orbit
      val orbits = result.orbits
      for (i <- 0 until n / 2) {
        withClue(s"P$n vertices $i and ${n - 1 - i}: ") {
          orbits(i) shouldBe orbits(n - 1 - i)
        }
      }

      // Number of orbits = ceil(n/2)
      withClue(s"P$n num orbits: ") {
        result.numOrbits shouldBe (n + 1) / 2
      }
    }
  }

  it should "put all vertices in one orbit for complete graphs" in {
    for (n <- 3 to 6) {
      val g = SparseGraph.complete(n)
      val result = SparseNauty.sparsenauty(g)

      result.numOrbits shouldBe 1

      // All vertices should have the same orbit representative
      val rep = result.orbits(0)
      for (i <- 1 until n) {
        result.orbits(i) shouldBe rep
      }
    }
  }

  //
  // EDGE CASES
  //

  "Sparse nauty" should "handle disconnected graphs" in {
    // Two K3 components
    val edges = Seq((0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3))
    val g = SparseGraph.fromEdges(6, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    // Aut group is S3 x S3 x S2 (swap components) = 6 * 6 * 2 = 72
    result.groupSize shouldBe BigDecimal(72)
  }

  it should "handle graph with isolated vertices" in {
    // Triangle + 2 isolated vertices
    val edges = Seq((0, 1), (1, 2), (2, 0))
    val g = SparseGraph.fromEdges(5, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    // Aut group is S3 (triangle) x S2 (isolated pair) = 6 * 2 = 12
    result.groupSize shouldBe BigDecimal(12)
  }

  it should "handle graphs with self-loops" in {
    // K2 with a self-loop on vertex 0
    val builder = new com.thatdot.nauty.graph.SparseGraphBuilder(2, 4, directed = true)
    builder.addArc(0, 1)
    builder.addArc(1, 0)
    builder.addArc(0, 0)  // Self-loop
    val g = builder.build()

    // With self-loop, the two vertices are distinguishable
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseDigraph.withSchreier)
    result.groupSize shouldBe BigDecimal(1)  // Only identity automorphism
    result.numOrbits shouldBe 2
  }

  //
  // DIRECTED GRAPHS
  //

  "Sparse nauty for digraphs" should "handle directed cycles" in {
    // Directed C_4: 0->1->2->3->0
    val edges = Seq((0, 1), (1, 2), (2, 3), (3, 0))
    val g = SparseGraph.fromEdges(4, edges, directed = true)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseDigraph.withSchreier)

    // Only rotations are automorphisms (no reflections), so |Aut| = 4
    result.groupSize shouldBe BigDecimal(4)
  }

  it should "handle directed paths" in {
    // Directed P_4: 0->1->2->3
    val edges = Seq((0, 1), (1, 2), (2, 3))
    val g = SparseGraph.fromEdges(4, edges, directed = true)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseDigraph.withSchreier)

    // Only identity automorphism
    result.groupSize shouldBe BigDecimal(1)
    result.numOrbits shouldBe 4
  }

  //
  // RANDOM GRAPH CONSISTENCY
  //

  "Random graphs" should "give consistent canonical forms" in {
    val rng = new Random(42)

    for (_ <- 1 to 20) {
      val n = 6 + rng.nextInt(5)  // 6-10 vertices
      val edgeProb = 0.3 + rng.nextDouble() * 0.4  // 30-70% edge probability

      val edges = for {
        i <- 0 until n
        j <- i + 1 until n
        if rng.nextDouble() < edgeProb
      } yield (i, j)

      val base = SparseGraph.fromEdges(n, edges)
      val opts = NautyOptions.defaultSparseGraph.withCanon

      val baseResult = SparseNauty.sparsenauty(base, opts)
      val baseCanon = baseResult.canonicalGraph.get

      // Check that a permutation gives the same canonical form
      val (permuted, _) = permuteGraph(base, rng.nextLong())
      val permResult = SparseNauty.sparsenauty(permuted, opts)
      val permCanon = permResult.canonicalGraph.get

      baseCanon shouldBe permCanon
    }
  }

  //
  // LARGE GRAPH TESTS
  //

  "Large complete graphs" should "compute correct group sizes" in {
    // K10: |Aut| = 10! = 3628800
    val k10 = SparseGraph.complete(10)
    val result = SparseNauty.sparsenauty(k10, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(3628800)
  }

  "Large cycle graphs" should "compute correct group sizes" in {
    // C20: |Aut| = 40
    val c20 = SparseGraph.cycle(20)
    val result = SparseNauty.sparsenauty(c20, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(40)
  }

  //
  // WHEEL GRAPHS - Known group sizes
  //

  "Wheel graph W_n" should "have automorphism group D_n for n >= 4" in {
    // Note: W_3 = K_4 (since C_3 is complete), so Aut(W_3) = S_4, not D_3
    // The D_n formula only applies for n >= 4
    for (n <- 4 to 8) {
      // Wheel with hub 0 and rim 1..n
      val rimEdges = (1 to n).map(i => (i, if (i == n) 1 else i + 1))
      val spokeEdges = (1 to n).map(i => (0, i))
      val g = SparseGraph.fromEdges(n + 1, rimEdges ++ spokeEdges)

      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

      withClue(s"W_$n group size: ") {
        result.groupSize shouldBe BigDecimal(2 * n)
      }
    }
  }

  //
  // PRISM GRAPHS
  //

  "Prism graph Y_n" should "have correct automorphism group for n >= 3" in {
    // Y_n has 2n vertices: outer ring 0..n-1, inner ring n..2n-1
    // For n != 4: Aut(Y_n) = D_n x Z_2, order 4n
    // For n = 4: Y_4 = C_4 □ K_2 ≅ Q_3 (3-cube), Aut(Q_3) has order 2^3 * 3! = 48
    for (n <- 3 to 6) {
      val outerRing = (0 until n).map(i => (i, (i + 1) % n))
      val innerRing = (0 until n).map(i => (n + i, n + (i + 1) % n))
      val spokes = (0 until n).map(i => (i, n + i))
      val g = SparseGraph.fromEdges(2 * n, outerRing ++ innerRing ++ spokes)

      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

      val expected = if (n == 4) 48 else 4 * n
      withClue(s"Y_$n group size: ") {
        result.groupSize shouldBe BigDecimal(expected)
      }
    }
  }
}

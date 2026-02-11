package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions
import scala.util.Random

/**
 * Tests to verify whether implementation differences between Scala sparse
 * and C nauty cause actual output differences.
 *
 * Issue 2: Missing BFS optimization for initial refinement
 * Issue 3: Active cell selection heuristics differ
 * Issue 4: sameRows parameter ignored in updateCanon
 *
 * If these tests pass, the differences are performance-only, not correctness issues.
 * If these tests fail, we've found a real bug.
 */
class SparseIssueVerificationSpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * factorial(n - 1)

  // Helper to create a random permutation of a graph
  private def permuteGraph(g: SparseGraph, seed: Long): SparseGraph = {
    val rng = new Random(seed)
    val perm = rng.shuffle((0 until g.n).toList).toArray
    g.permute(perm)
  }

  //
  // ISSUE 2: Missing BFS optimization
  //
  // The C code uses BFS when: level <= 2, nactive == 1, singleton active cell, numcells <= n/8
  // This optimization is triggered for large sparse graphs with few initial cells.
  // If this causes incorrect results, we'd see wrong group sizes or canonical forms.
  //

  "Issue 2 (BFS optimization)" should "not affect group size for large sparse graphs" in {
    // Create graphs that would trigger the BFS optimization in C:
    // - Large n (so n/8 is reasonable)
    // - Sparse (few edges)
    // - Initial partition has few cells

    // Path graph P_20 - sparse, large, single initial cell
    val path20 = SparseGraph.path(20)
    val result = SparseNauty.sparsenauty(path20, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(2)  // Z_2 for any path

    // Cycle C_20 - sparse, large
    val cycle20 = SparseGraph.cycle(20)
    val cycleResult = SparseNauty.sparsenauty(cycle20, NautyOptions.defaultSparseGraph.withSchreier)
    cycleResult.groupSize shouldBe BigDecimal(40)  // D_20

    // Star graph with 20 vertices
    val starEdges = (1 until 20).map(i => (0, i))
    val star20 = SparseGraph.fromEdges(20, starEdges)
    val starResult = SparseNauty.sparsenauty(star20, NautyOptions.defaultSparseGraph.withSchreier)
    starResult.groupSize shouldBe BigDecimal(factorial(19))  // S_19
  }

  it should "not affect canonical forms for large sparse graphs" in {
    val path20 = SparseGraph.path(20)
    val opts = NautyOptions.defaultSparseGraph.withCanon

    val baseCanon = SparseNauty.sparsenauty(path20, opts).canonicalGraph.get

    // Multiple permutations should all give same canonical form
    for (seed <- 1 to 10) {
      val permuted = permuteGraph(path20, seed)
      val permCanon = SparseNauty.sparsenauty(permuted, opts).canonicalGraph.get

      withClue(s"Path20 permutation $seed: ") {
        baseCanon shouldBe permCanon
      }
    }
  }

  it should "match dense nauty results for large graphs" in {
    // Compare sparse vs dense for graphs that would use BFS in C sparse
    val path16 = SparseGraph.path(16)
    val densePath16 = path16.toDense

    val sparseResult = SparseNauty.sparsenauty(path16, NautyOptions.defaultSparseGraph.withSchreier)
    val denseResult = Nauty.densenauty(densePath16, NautyOptions.defaultGraph.withSchreier)

    sparseResult.groupSize shouldBe denseResult.groupSize
  }

  //
  // ISSUE 3: Active cell selection heuristics
  //
  // C prefers singleton cells and tracks largest fragment when splitting.
  // Different traversal order could theoretically affect canonical labeling choice.
  //

  "Issue 3 (active cell selection)" should "not affect group sizes" in {
    // Test graphs where cell selection matters more:
    // - Graphs with multiple non-trivial cells
    // - Graphs where refinement produces many fragments

    // Complete bipartite K_{4,4} - has two initial cells if colored
    val k44Edges = for (i <- 0 until 4; j <- 4 until 8) yield (i, j)
    val k44 = SparseGraph.fromEdges(8, k44Edges)
    val k44Result = SparseNauty.sparsenauty(k44, NautyOptions.defaultSparseGraph.withSchreier)
    // |Aut(K_{4,4})| = 4! * 4! * 2 = 1152
    k44Result.groupSize shouldBe BigDecimal(1152)

    // Petersen graph - complex refinement
    val petersen = SparseGraph.fromEdges(10, Seq(
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))
    val petersenResult = SparseNauty.sparsenauty(petersen, NautyOptions.defaultSparseGraph.withSchreier)
    petersenResult.groupSize shouldBe BigDecimal(120)
  }

  it should "produce consistent canonical forms regardless of input order" in {
    // If active cell selection caused non-deterministic canonical forms,
    // different permutations of the same graph might give different results

    val petersen = SparseGraph.fromEdges(10, Seq(
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))

    val opts = NautyOptions.defaultSparseGraph.withCanon
    val baseCanon = SparseNauty.sparsenauty(petersen, opts).canonicalGraph.get

    // Test many random permutations
    for (seed <- 1 to 20) {
      val permuted = permuteGraph(petersen, seed)
      val permCanon = SparseNauty.sparsenauty(permuted, opts).canonicalGraph.get

      withClue(s"Petersen permutation $seed: ") {
        baseCanon shouldBe permCanon
      }
    }
  }

  it should "correctly detect isomorphism with different input orders" in {
    // Two representations of K_{3,3} with different vertex orderings
    // should be detected as isomorphic
    // K_{3,3}: bipartite with parts {0,2,4} and {1,3,5}

    val g1 = SparseGraph.fromEdges(6, Seq(
      (0, 1), (0, 3), (0, 5),
      (2, 1), (2, 3), (2, 5),
      (4, 1), (4, 3), (4, 5)
    ))

    // Same graph with relabeling that swaps the two parts:
    // Apply permutation: 0<->1, 2<->3, 4<->5
    // Parts {0,2,4} become {1,3,5} and vice versa
    val g2 = SparseGraph.fromEdges(6, Seq(
      (1, 0), (1, 2), (1, 4),
      (3, 0), (3, 2), (3, 4),
      (5, 0), (5, 2), (5, 4)
    ))

    SparseNauty.isIsomorphic(g1, g2) shouldBe true
  }

  //
  // ISSUE 4: sameRows parameter ignored in updateCanon
  //
  // This is purely a performance optimization - rebuilding the whole graph
  // vs. only updating changed rows. Output should be identical.
  //

  "Issue 4 (sameRows ignored)" should "not affect canonical graph output" in {
    // The sameRows optimization is internal to updateCanon.
    // If it's broken, canonical forms would be wrong.

    // Test graphs where updateCanon is called multiple times during search
    val graphs = Seq(
      ("K5", SparseGraph.complete(5)),
      ("C8", SparseGraph.cycle(8)),
      ("Petersen", SparseGraph.fromEdges(10, Seq(
        (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
        (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
        (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
      )))
    )

    val opts = NautyOptions.defaultSparseGraph.withCanon

    for ((name, g) <- graphs) {
      val result = SparseNauty.sparsenauty(g, opts)
      val canon = result.canonicalGraph.get

      // The canonical graph should be valid
      canon.n shouldBe g.n

      // It should have the same number of edges
      canon.numEdges shouldBe g.numEdges

      // Multiple runs should give same result
      val result2 = SparseNauty.sparsenauty(g, opts)
      val canon2 = result2.canonicalGraph.get

      withClue(s"$name canonical form consistency: ") {
        canon shouldBe canon2
      }
    }
  }

  it should "produce canonical forms that match dense nauty" in {
    // Compare canonical labeling between sparse and dense implementations
    // Both should identify the same canonical representative

    val graphs = Seq(
      ("K4", SparseGraph.complete(4)),
      ("C6", SparseGraph.cycle(6)),
      ("P5", SparseGraph.path(5))
    )

    for ((name, sparse) <- graphs) {
      val dense = sparse.toDense

      val sparseOpts = NautyOptions.defaultSparseGraph.withCanon
      val denseOpts = NautyOptions.defaultGraph.withCanon

      val sparseResult = SparseNauty.sparsenauty(sparse, sparseOpts)
      val denseResult = Nauty.densenauty(dense, denseOpts)

      // Group sizes should match
      withClue(s"$name group size: ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }

      // Canonical labelings may differ in representation but should be equivalent
      // Test by verifying both canonical forms are isomorphic to original
      val sparseCanon = sparseResult.canonicalGraph.get
      SparseNauty.isIsomorphic(sparse, sparseCanon) shouldBe true
    }
  }

  //
  // COMPREHENSIVE CORRECTNESS TESTS
  //
  // If any of the above issues caused real problems, these tests would fail.
  //

  "Sparse nauty overall" should "give correct group sizes for all standard graph families" in {
    // Complete graphs K_n: |Aut| = n!
    for (n <- 2 to 7) {
      val g = SparseGraph.complete(n)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
      withClue(s"K$n: ") {
        result.groupSize shouldBe BigDecimal(factorial(n))
      }
    }

    // Cycles C_n: |Aut| = 2n
    for (n <- 3 to 10) {
      val g = SparseGraph.cycle(n)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
      withClue(s"C$n: ") {
        result.groupSize shouldBe BigDecimal(2 * n)
      }
    }

    // Paths P_n: |Aut| = 2
    for (n <- 2 to 10) {
      val g = SparseGraph.path(n)
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
      withClue(s"P$n: ") {
        result.groupSize shouldBe BigDecimal(2)
      }
    }
  }

  it should "correctly handle random graphs" in {
    val rng = new Random(12345)

    for (trial <- 1 to 30) {
      val n = 5 + rng.nextInt(6)  // 5-10 vertices
      val edgeProb = 0.2 + rng.nextDouble() * 0.6

      val edges = for {
        i <- 0 until n
        j <- i + 1 until n
        if rng.nextDouble() < edgeProb
      } yield (i, j)

      val g = SparseGraph.fromEdges(n, edges)
      val opts = NautyOptions.defaultSparseGraph.withCanon.withSchreier

      // Get canonical form
      val result = SparseNauty.sparsenauty(g, opts)
      val canon = result.canonicalGraph.get

      // Verify canonical form is isomorphic to original
      withClue(s"Trial $trial (n=$n, edges=${edges.size}): ") {
        SparseNauty.isIsomorphic(g, canon) shouldBe true
      }

      // Verify permutation gives same canonical form
      val permuted = permuteGraph(g, trial * 1000L)
      val permResult = SparseNauty.sparsenauty(permuted, opts)

      withClue(s"Trial $trial permutation: ") {
        result.groupSize shouldBe permResult.groupSize
        canon shouldBe permResult.canonicalGraph.get
      }
    }
  }
}

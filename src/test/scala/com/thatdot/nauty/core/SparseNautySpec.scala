package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.{SparseGraph, DenseGraph}
import com.thatdot.nauty.util.NautyOptions
import com.thatdot.nauty.group.Permutation

class SparseNautySpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): Int = if (n <= 1) 1 else n * factorial(n - 1)

  private def isAutomorphism(g: SparseGraph, perm: Permutation): Boolean = {
    val n = g.n
    var i = 0
    var isAuto = true
    while (i < n && isAuto) {
      val neighbors = g.neighbors(i)
      var idx = 0
      while (idx < neighbors.length && isAuto) {
        if (!g.hasEdge(perm(i), perm(neighbors(idx)))) isAuto = false
        idx += 1
      }
      i += 1
    }
    isAuto
  }

  //
  // Basic functionality tests
  //

  "SparseNauty.sparsenauty" should "handle empty graph" in {
    val g = SparseGraph.empty(0)
    val result = SparseNauty.sparsenauty(g)

    result.numOrbits shouldBe 0
    result.generators shouldBe empty
    result.groupSize shouldBe BigDecimal(1)
  }

  it should "handle single vertex" in {
    val g = SparseGraph.empty(1)
    val result = SparseNauty.sparsenauty(g)

    result.numOrbits shouldBe 1
    result.generators shouldBe empty
  }

  it should "handle single edge" in {
    val g = SparseGraph.fromEdges(2, Seq((0, 1)))
    val result = SparseNauty.sparsenauty(g)

    // K2 has Z2 automorphism group
    result.groupSize shouldBe BigDecimal(2)
    result.numOrbits shouldBe 1
  }

  //
  // Group order tests
  //

  "Complete graph K3" should "have automorphism group S3 of order 6" in {
    val g = SparseGraph.complete(3)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    result.groupSize shouldBe BigDecimal(6)
  }

  "Complete graph K4" should "have automorphism group S4 of order 24" in {
    val g = SparseGraph.complete(4)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    result.groupSize shouldBe BigDecimal(24)
  }

  "Complete graph K5" should "have automorphism group S5 of order 120" in {
    val g = SparseGraph.complete(5)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    result.groupSize shouldBe BigDecimal(120)
  }

  "Cycle graph C4" should "have automorphism group D4 of order 8" in {
    val g = SparseGraph.cycle(4)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    result.groupSize shouldBe BigDecimal(8)
  }

  "Cycle graph C5" should "have automorphism group D5 of order 10" in {
    val g = SparseGraph.cycle(5)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    result.groupSize shouldBe BigDecimal(10)
  }

  "Path graph P4" should "have automorphism group Z2 of order 2" in {
    val g = SparseGraph.path(4)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    result.groupSize shouldBe BigDecimal(2)
    result.numOrbits shouldBe 2
  }

  //
  // Generator validity tests
  //

  "Sparse nauty generators" should "all be valid automorphisms" in {
    val graphs = Seq(
      SparseGraph.complete(4),
      SparseGraph.cycle(5),
      SparseGraph.path(4),
      SparseGraph.fromEdges(4, Seq((0, 1), (0, 2), (0, 3)))  // Star
    )

    for (g <- graphs) {
      val result = SparseNauty.sparsenauty(g)
      for (gen <- result.generators) {
        withClue(s"Generator ${gen.toCycleString} for graph with ${g.n} vertices: ") {
          isAutomorphism(g, gen) shouldBe true
        }
      }
    }
  }

  //
  // Canonical form tests
  //

  "SparseNauty.canonicalForm" should "produce same result for isomorphic graphs" in {
    val g1 = SparseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3)))
    val g2 = SparseGraph.fromEdges(4, Seq((3, 2), (2, 1), (1, 0)))

    val opts = NautyOptions.defaultSparseGraph.withCanon
    val r1 = SparseNauty.sparsenauty(g1, opts)
    val r2 = SparseNauty.sparsenauty(g2, opts)

    r1.canonicalGraph shouldBe defined
    r2.canonicalGraph shouldBe defined
    r1.canonicalGraph.get shouldBe r2.canonicalGraph.get
  }

  //
  // Isomorphism tests
  //

  "SparseNauty.isIsomorphic" should "detect isomorphic graphs" in {
    val g1 = SparseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3), (3, 0)))
    val g2 = SparseGraph.fromEdges(4, Seq((0, 2), (2, 1), (1, 3), (3, 0)))

    SparseNauty.isIsomorphic(g1, g2) shouldBe true
  }

  it should "detect non-isomorphic graphs" in {
    val g1 = SparseGraph.cycle(6)
    val g2 = SparseGraph.fromEdges(6, Seq((0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3)))

    SparseNauty.isIsomorphic(g1, g2) shouldBe false
  }

  //
  // Consistency with dense nauty
  //

  "Sparse nauty" should "give same group size as dense nauty for complete graphs" in {
    for (n <- 3 to 6) {
      val sparse = SparseGraph.complete(n)
      val dense = DenseGraph.complete(n)

      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)

      withClue(s"K$n: ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }

  it should "give same group size as dense nauty for cycles" in {
    for (n <- 3 to 8) {
      val sparse = SparseGraph.cycle(n)
      val dense = DenseGraph.cycle(n)

      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)

      withClue(s"C$n: ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }

  it should "give same group size as dense nauty for paths" in {
    for (n <- 2 to 6) {
      val sparse = SparseGraph.path(n)
      val dense = DenseGraph.path(n)

      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)

      withClue(s"P$n: ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }

  //
  // Petersen graph test
  //

  "Petersen graph" should "have automorphism group of order 120" in {
    val petersen = SparseGraph.fromEdges(10, Seq(
      // Outer pentagon
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      // Inner pentagram
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      // Spokes
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))

    val result = SparseNauty.sparsenauty(petersen, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(120)
  }

  //
  // Integration with Nauty.sparsenauty
  //

  "Nauty.sparsenauty" should "use native sparse implementation" in {
    val g = SparseGraph.cycle(5)
    val result = Nauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)

    result.groupSize shouldBe BigDecimal(10)
    result.numOrbits shouldBe 1
  }

  it should "return canonical form as sparse graph" in {
    val g = SparseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3)))
    val result = Nauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withCanon)

    result.canonicalGraph shouldBe defined
    result.canonicalGraph.get.n shouldBe 4
    // Verify it's a SparseGraph, not converted to dense
    result.canonicalGraph.get shouldBe a[SparseGraph]
  }
}

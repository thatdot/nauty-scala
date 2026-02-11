package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

/**
 * Tests for group size calculation in sparse nauty.
 */
class GroupSizeBugSpec extends AnyFlatSpec with Matchers {

  "Sparse nauty group size" should "be correct WITH Schreier for star S6" in {
    val edges = (1 until 7).map(i => (0, i))
    val g = SparseGraph.fromEdges(7, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(720)  // 6!
  }

  it should "be correct WITHOUT Schreier for star S6" in {
    val edges = (1 until 7).map(i => (0, i))
    val g = SparseGraph.fromEdges(7, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
    result.groupSize shouldBe BigDecimal(720)  // 6!
  }

  it should "be correct WITH Schreier for K5" in {
    val g = SparseGraph.complete(5)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(120)  // 5!
  }

  it should "be correct WITHOUT Schreier for K5" in {
    val g = SparseGraph.complete(5)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
    result.groupSize shouldBe BigDecimal(120)  // 5!
  }

  it should "be correct WITH Schreier for C6" in {
    val g = SparseGraph.cycle(6)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(12)  // D_6 = 2*6
  }

  it should "be correct WITHOUT Schreier for C6" in {
    val g = SparseGraph.cycle(6)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
    result.groupSize shouldBe BigDecimal(12)  // D_6 = 2*6
  }

  // Compare sparse vs dense for multiple graphs
  "Sparse vs Dense group sizes" should "match for star graphs" in {
    for (n <- 3 to 8) {
      val edges = (1 until n).map(i => (0, i))
      val sparse = SparseGraph.fromEdges(n, edges)
      val dense = sparse.toDense

      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph)

      withClue(s"Star S${n-1} (n=$n vertices): ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }

  it should "match for complete graphs" in {
    for (n <- 3 to 6) {
      val sparse = SparseGraph.complete(n)
      val dense = sparse.toDense

      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph)

      withClue(s"Complete K$n: ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }

  it should "match for cycle graphs" in {
    for (n <- 3 to 8) {
      val sparse = SparseGraph.cycle(n)
      val dense = sparse.toDense

      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph)

      withClue(s"Cycle C$n: ") {
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }
}

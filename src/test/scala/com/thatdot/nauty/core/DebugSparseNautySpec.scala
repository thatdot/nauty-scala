package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

/**
 * Tests for sparse nauty consistency.
 */
class DebugSparseNautySpec extends AnyFlatSpec with Matchers {

  "Debug" should "show what's happening in S7" in {
    // Create star graph S7 (8 vertices: center=0, leaves=1-7)
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)

    // Run multiple times and verify consistency
    for (_ <- 1 to 5) {
      val opts = NautyOptions.defaultSparseGraph.withSchreier
      val result = SparseNauty.sparsenauty(g, opts)
      result.groupSize shouldBe BigDecimal(5040)
    }
  }

  it should "compare sparse vs dense for S7" in {
    val edges = (1 until 8).map(i => (0, i))
    val sparse = SparseGraph.fromEdges(8, edges)
    val dense = sparse.toDense

    for (_ <- 1 to 3) {
      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)

      sparseResult.groupSize shouldBe BigDecimal(5040)
      denseResult.groupSize shouldBe BigDecimal(5040)
    }
  }

  it should "test without Schreier to isolate the bug" in {
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)

    for (_ <- 1 to 5) {
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
      result.groupSize shouldBe BigDecimal(5040)
    }
  }
}

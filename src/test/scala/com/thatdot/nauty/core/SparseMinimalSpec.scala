package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

class SparseMinimalSpec extends AnyFlatSpec with Matchers {

  "Sparse nauty" should "handle K3" in {
    val g = SparseGraph.complete(3)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(6)
  }

  it should "handle K4" in {
    val g = SparseGraph.complete(4)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(24)
  }

  it should "handle C5" in {
    val g = SparseGraph.cycle(5)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(10)
  }

  it should "handle P4" in {
    val g = SparseGraph.path(4)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(2)
  }

  it should "handle K5 with canon" in {
    val g = SparseGraph.complete(5)
    val opts = NautyOptions.defaultSparseGraph.withCanon
    val result = SparseNauty.sparsenauty(g, opts)
    result.canonicalGraph shouldBe defined
  }

  it should "handle Petersen graph" in {
    val petersen = SparseGraph.fromEdges(10, Seq(
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))
    val result = SparseNauty.sparsenauty(petersen, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(120)
  }

  it should "handle path P20" in {
    val g = SparseGraph.path(20)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(2)
  }
}

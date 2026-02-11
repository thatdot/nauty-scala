package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

class StarGraphDebugSpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * factorial(n - 1)

  "Star graph" should "handle S3 (4 vertices)" in {
    val edges = (1 until 4).map(i => (0, i))
    val g = SparseGraph.fromEdges(4, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(factorial(3))
  }

  it should "handle S4 (5 vertices)" in {
    val edges = (1 until 5).map(i => (0, i))
    val g = SparseGraph.fromEdges(5, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(factorial(4))
  }

  it should "handle S5 (6 vertices)" in {
    val edges = (1 until 6).map(i => (0, i))
    val g = SparseGraph.fromEdges(6, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(factorial(5))
  }

  it should "handle S6 (7 vertices)" in {
    val edges = (1 until 7).map(i => (0, i))
    val g = SparseGraph.fromEdges(7, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(factorial(6))
  }

  it should "handle S7 (8 vertices)" in {
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(factorial(7))
  }

  it should "handle S8 (9 vertices)" in {
    val edges = (1 until 9).map(i => (0, i))
    val g = SparseGraph.fromEdges(9, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    result.groupSize shouldBe BigDecimal(factorial(8))
  }

  it should "match dense nauty for S6" in {
    val edges = (1 until 7).map(i => (0, i))
    val sparse = SparseGraph.fromEdges(7, edges)
    val dense = sparse.toDense

    val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
    val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)

    sparseResult.groupSize shouldBe denseResult.groupSize
  }
}

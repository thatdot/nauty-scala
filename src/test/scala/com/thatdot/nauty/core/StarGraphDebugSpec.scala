package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

class StarGraphDebugSpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * factorial(n - 1)

  "Star graph" should "handle S3 (4 vertices)" in {
    println("Testing S3 (4 vertices)...")
    val edges = (1 until 4).map(i => (0, i))
    val g = SparseGraph.fromEdges(4, edges)
    val start = System.currentTimeMillis()
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    val time = System.currentTimeMillis() - start
    println(s"S3: groupSize=${result.groupSize}, time=${time}ms")
    result.groupSize shouldBe BigDecimal(factorial(3))
  }

  it should "handle S4 (5 vertices)" in {
    println("Testing S4 (5 vertices)...")
    val edges = (1 until 5).map(i => (0, i))
    val g = SparseGraph.fromEdges(5, edges)
    val start = System.currentTimeMillis()
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    val time = System.currentTimeMillis() - start
    println(s"S4: groupSize=${result.groupSize}, time=${time}ms")
    result.groupSize shouldBe BigDecimal(factorial(4))
  }

  it should "handle S5 (6 vertices)" in {
    println("Testing S5 (6 vertices)...")
    val edges = (1 until 6).map(i => (0, i))
    val g = SparseGraph.fromEdges(6, edges)
    val start = System.currentTimeMillis()
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    val time = System.currentTimeMillis() - start
    println(s"S5: groupSize=${result.groupSize}, time=${time}ms")
    result.groupSize shouldBe BigDecimal(factorial(5))
  }

  it should "handle S6 (7 vertices)" in {
    println("Testing S6 (7 vertices)...")
    val edges = (1 until 7).map(i => (0, i))
    val g = SparseGraph.fromEdges(7, edges)
    val start = System.currentTimeMillis()
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    val time = System.currentTimeMillis() - start
    println(s"S6: groupSize=${result.groupSize}, time=${time}ms")
    result.groupSize shouldBe BigDecimal(factorial(6))
  }

  it should "handle S7 (8 vertices)" in {
    println("Testing S7 (8 vertices)...")
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)
    val start = System.currentTimeMillis()
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    val time = System.currentTimeMillis() - start
    println(s"S7: groupSize=${result.groupSize}, time=${time}ms")
    result.groupSize shouldBe BigDecimal(factorial(7))
  }

  it should "handle S8 (9 vertices)" in {
    println("Testing S8 (9 vertices)...")
    val edges = (1 until 9).map(i => (0, i))
    val g = SparseGraph.fromEdges(9, edges)
    val start = System.currentTimeMillis()
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    val time = System.currentTimeMillis() - start
    println(s"S8: groupSize=${result.groupSize}, time=${time}ms")
    result.groupSize shouldBe BigDecimal(factorial(8))
  }

  // Compare with dense nauty to see if sparse is the problem
  it should "match dense nauty for S6" in {
    println("Comparing S6 sparse vs dense...")
    val edges = (1 until 7).map(i => (0, i))
    val sparse = SparseGraph.fromEdges(7, edges)
    val dense = sparse.toDense

    val startSparse = System.currentTimeMillis()
    val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
    val timeSparse = System.currentTimeMillis() - startSparse

    val startDense = System.currentTimeMillis()
    val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)
    val timeDense = System.currentTimeMillis() - startDense

    println(s"Sparse: ${sparseResult.groupSize}, time=${timeSparse}ms")
    println(s"Dense: ${denseResult.groupSize}, time=${timeDense}ms")

    sparseResult.groupSize shouldBe denseResult.groupSize
  }
}

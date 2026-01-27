package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

/**
 * Tests demonstrating group size calculation bug in sparse nauty.
 */
class GroupSizeBugSpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * factorial(n - 1)

  "Sparse nauty group size" should "be correct WITH Schreier for star S6" in {
    val edges = (1 until 7).map(i => (0, i))
    val g = SparseGraph.fromEdges(7, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    println(s"S6 WITH Schreier: groupSize=${result.groupSize}")
    result.groupSize shouldBe BigDecimal(720)  // 6!
  }

  it should "be correct WITHOUT Schreier for star S6" in {
    val edges = (1 until 7).map(i => (0, i))
    val g = SparseGraph.fromEdges(7, edges)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)  // No Schreier
    println(s"S6 WITHOUT Schreier: groupSize=${result.groupSize}")
    result.groupSize shouldBe BigDecimal(720)  // 6!
  }

  it should "be correct WITH Schreier for K5" in {
    val g = SparseGraph.complete(5)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    println(s"K5 WITH Schreier: groupSize=${result.groupSize}")
    result.groupSize shouldBe BigDecimal(120)  // 5!
  }

  it should "be correct WITHOUT Schreier for K5" in {
    val g = SparseGraph.complete(5)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
    println(s"K5 WITHOUT Schreier: groupSize=${result.groupSize}")
    result.groupSize shouldBe BigDecimal(120)  // 5!
  }

  it should "be correct WITH Schreier for C6" in {
    val g = SparseGraph.cycle(6)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
    println(s"C6 WITH Schreier: groupSize=${result.groupSize}")
    result.groupSize shouldBe BigDecimal(12)  // D_6 = 2*6
  }

  it should "be correct WITHOUT Schreier for C6" in {
    val g = SparseGraph.cycle(6)
    val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
    println(s"C6 WITHOUT Schreier: groupSize=${result.groupSize}")
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
        println(s"S${n-1}: sparse=${sparseResult.groupSize}, dense=${denseResult.groupSize}")
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
        println(s"K$n: sparse=${sparseResult.groupSize}, dense=${denseResult.groupSize}")
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
        println(s"C$n: sparse=${sparseResult.groupSize}, dense=${denseResult.groupSize}")
        sparseResult.groupSize shouldBe denseResult.groupSize
      }
    }
  }
}

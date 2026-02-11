package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

/**
 * Tests for consistency in sparse nauty.
 */
class NonDeterministicBugSpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * factorial(n - 1)

  "Sparse nauty" should "give consistent results for star S7 across multiple runs" in {
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)
    val expected = BigDecimal(factorial(7))

    var allCorrect = true
    for (_ <- 1 to 10) {
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
      if (result.groupSize != expected) allCorrect = false
    }

    allCorrect shouldBe true
  }

  it should "give consistent results for star S6 across multiple runs" in {
    val edges = (1 until 7).map(i => (0, i))
    val g = SparseGraph.fromEdges(7, edges)
    val expected = BigDecimal(factorial(6))

    var allCorrect = true
    for (_ <- 1 to 10) {
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph.withSchreier)
      if (result.groupSize != expected) allCorrect = false
    }

    allCorrect shouldBe true
  }

  it should "give consistent results running both S6 and S7 interleaved" in {
    val s6edges = (1 until 7).map(i => (0, i))
    val s6 = SparseGraph.fromEdges(7, s6edges)
    val s6expected = BigDecimal(factorial(6))

    val s7edges = (1 until 8).map(i => (0, i))
    val s7 = SparseGraph.fromEdges(8, s7edges)
    val s7expected = BigDecimal(factorial(7))

    var allCorrect = true
    for (_ <- 1 to 5) {
      val r6 = SparseNauty.sparsenauty(s6, NautyOptions.defaultSparseGraph.withSchreier)
      val r7 = SparseNauty.sparsenauty(s7, NautyOptions.defaultSparseGraph.withSchreier)

      if (r6.groupSize != s6expected || r7.groupSize != s7expected) allCorrect = false
    }

    allCorrect shouldBe true
  }

  it should "give correct results for K3 followed by S7" in {
    val k3 = SparseGraph.complete(3)
    val k3result = SparseNauty.sparsenauty(k3, NautyOptions.defaultSparseGraph.withSchreier)

    val s7edges = (1 until 8).map(i => (0, i))
    val s7 = SparseGraph.fromEdges(8, s7edges)
    val s7result = SparseNauty.sparsenauty(s7, NautyOptions.defaultSparseGraph.withSchreier)

    k3result.groupSize shouldBe BigDecimal(6)
    s7result.groupSize shouldBe BigDecimal(5040)
  }

  it should "give correct results for S7 alone" in {
    val s7edges = (1 until 8).map(i => (0, i))
    val s7 = SparseGraph.fromEdges(8, s7edges)
    val s7result = SparseNauty.sparsenauty(s7, NautyOptions.defaultSparseGraph.withSchreier)

    s7result.groupSize shouldBe BigDecimal(5040)
  }
}

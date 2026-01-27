package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions
import scala.util.Random

class SparseHangDebugSpec extends AnyFlatSpec with Matchers {

  private def permuteGraph(g: SparseGraph, seed: Long): SparseGraph = {
    val rng = new Random(seed)
    val perm = rng.shuffle((0 until g.n).toList).toArray
    g.permute(perm)
  }

  "Sparse nauty" should "handle isomorphism check" in {
    println("Testing isomorphism...")
    val g1 = SparseGraph.complete(4)
    val g2 = SparseGraph.complete(4)
    val result = SparseNauty.isIsomorphic(g1, g2)
    println(s"Isomorphism result: $result")
    result shouldBe true
  }

  it should "handle canonical form comparison" in {
    println("Testing canon comparison for K4...")
    val g1 = SparseGraph.complete(4)
    val opts = NautyOptions.defaultSparseGraph.withCanon

    val r1 = SparseNauty.sparsenauty(g1, opts)
    println(s"Got canon 1: ${r1.canonicalGraph.isDefined}")

    val g2 = permuteGraph(g1, 42)
    println("Created permuted graph")

    val r2 = SparseNauty.sparsenauty(g2, opts)
    println(s"Got canon 2: ${r2.canonicalGraph.isDefined}")

    r1.canonicalGraph.get shouldBe r2.canonicalGraph.get
    println("Canon comparison passed")
  }

  it should "handle star graph S19" in {
    println("Testing S19...")
    val starEdges = (1 until 20).map(i => (0, i))
    val star20 = SparseGraph.fromEdges(20, starEdges)
    val result = SparseNauty.sparsenauty(star20, NautyOptions.defaultSparseGraph.withSchreier)
    println(s"S19 groupSize=${result.groupSize}")
    // S_19 = 19!
    val expected = (1 to 19).map(BigDecimal(_)).product
    result.groupSize shouldBe expected
  }

  it should "handle K44" in {
    println("Testing K44...")
    val k44Edges = for (i <- 0 until 4; j <- 4 until 8) yield (i, j)
    val k44 = SparseGraph.fromEdges(8, k44Edges)
    val result = SparseNauty.sparsenauty(k44, NautyOptions.defaultSparseGraph.withSchreier)
    println(s"K44 groupSize=${result.groupSize}")
    // |Aut(K_{4,4})| = 4! * 4! * 2 = 1152
    result.groupSize shouldBe BigDecimal(1152)
  }

  it should "handle random graph" in {
    println("Testing random graph...")
    val rng = new Random(12345)
    val n = 8
    val edges = for {
      i <- 0 until n
      j <- i + 1 until n
      if rng.nextDouble() < 0.4
    } yield (i, j)

    println(s"Created random graph with n=$n, edges=${edges.size}")
    val g = SparseGraph.fromEdges(n, edges)
    val opts = NautyOptions.defaultSparseGraph.withCanon.withSchreier

    val result = SparseNauty.sparsenauty(g, opts)
    println(s"Random graph groupSize=${result.groupSize}")

    val canon = result.canonicalGraph.get
    println(s"Got canonical form")

    val isIso = SparseNauty.isIsomorphic(g, canon)
    println(s"Isomorphism check: $isIso")
    isIso shouldBe true
  }

  it should "handle permuted random graph" in {
    println("Testing permuted random graph...")
    val rng = new Random(12345)
    val n = 8
    val edges = for {
      i <- 0 until n
      j <- i + 1 until n
      if rng.nextDouble() < 0.4
    } yield (i, j)

    val g = SparseGraph.fromEdges(n, edges)
    val opts = NautyOptions.defaultSparseGraph.withCanon.withSchreier

    val result = SparseNauty.sparsenauty(g, opts)
    val canon = result.canonicalGraph.get

    println("Permuting graph...")
    val permuted = permuteGraph(g, 999)

    println("Computing canon of permuted...")
    val permResult = SparseNauty.sparsenauty(permuted, opts)
    val permCanon = permResult.canonicalGraph.get

    println("Comparing canons...")
    canon shouldBe permCanon
    println("Done!")
  }
}

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
    val g1 = SparseGraph.complete(4)
    val g2 = SparseGraph.complete(4)
    SparseNauty.isIsomorphic(g1, g2) shouldBe true
  }

  it should "handle canonical form comparison" in {
    val g1 = SparseGraph.complete(4)
    val opts = NautyOptions.defaultSparseGraph.withCanon

    val r1 = SparseNauty.sparsenauty(g1, opts)
    val g2 = permuteGraph(g1, 42)
    val r2 = SparseNauty.sparsenauty(g2, opts)

    r1.canonicalGraph.get shouldBe r2.canonicalGraph.get
  }

  it should "handle star graph S19" in {
    val starEdges = (1 until 20).map(i => (0, i))
    val star20 = SparseGraph.fromEdges(20, starEdges)
    val result = SparseNauty.sparsenauty(star20, NautyOptions.defaultSparseGraph.withSchreier)
    // S_19 = 19!
    val expected = (1 to 19).map(BigDecimal(_)).product
    result.groupSize shouldBe expected
  }

  it should "handle K44" in {
    val k44Edges = for (i <- 0 until 4; j <- 4 until 8) yield (i, j)
    val k44 = SparseGraph.fromEdges(8, k44Edges)
    val result = SparseNauty.sparsenauty(k44, NautyOptions.defaultSparseGraph.withSchreier)
    // |Aut(K_{4,4})| = 4! * 4! * 2 = 1152
    result.groupSize shouldBe BigDecimal(1152)
  }

  it should "handle random graph" in {
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

    SparseNauty.isIsomorphic(g, canon) shouldBe true
  }

  it should "handle permuted random graph" in {
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

    val permuted = permuteGraph(g, 999)
    val permResult = SparseNauty.sparsenauty(permuted, opts)
    val permCanon = permResult.canonicalGraph.get

    canon shouldBe permCanon
  }
}

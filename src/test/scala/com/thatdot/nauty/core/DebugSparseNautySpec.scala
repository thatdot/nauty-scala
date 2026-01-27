package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.{NautyOptions, AutomorphismCallback}
import com.thatdot.nauty.group.Orbits

/**
 * Debug test to understand the non-determinism in sparse nauty.
 */
class DebugSparseNautySpec extends AnyFlatSpec with Matchers {

  "Debug" should "show what's happening in S7" in {
    // Create star graph S7 (8 vertices: center=0, leaves=1-7)
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)

    println(s"Graph: n=${g.n}, edges=${edges.mkString(", ")}")

    // Run multiple times and track details
    for (run <- 1 to 5) {
      println(s"\n=== Run $run ===")

      // Use a custom callback to track automorphisms
      var autoCount = 0

      val callback: AutomorphismCallback = new AutomorphismCallback {
        def apply(count: Int, perm: Array[Int], orbits: Array[Int],
                  numOrbits: Int, numFixed: Int, n: Int): Unit = {
          autoCount += 1
          val permStr = perm.mkString(",")
          val orbitsStr = orbits.mkString(",")
          println(s"  Auto $autoCount: perm=[$permStr], orbits=[$orbitsStr], numOrbits=$numOrbits, numFixed=$numFixed")
        }
      }

      val opts = NautyOptions.defaultSparseGraph.withSchreier.copy(userAutomProc = Some(callback))
      val result = SparseNauty.sparsenauty(g, opts)

      println(s"  Final: groupSize=${result.groupSize}, numGenerators=${result.generators.size}, numOrbits=${result.numOrbits}")
      println(s"  Generators: ${result.generators.map(_.toCycleString).mkString("; ")}")
    }
  }

  it should "compare sparse vs dense for S7" in {
    val edges = (1 until 8).map(i => (0, i))
    val sparse = SparseGraph.fromEdges(8, edges)
    val dense = sparse.toDense

    for (run <- 1 to 3) {
      val sparseResult = SparseNauty.sparsenauty(sparse, NautyOptions.defaultSparseGraph.withSchreier)
      val denseResult = Nauty.densenauty(dense, NautyOptions.defaultGraph.withSchreier)

      val sparseOK = if (sparseResult.groupSize == BigDecimal(5040)) "OK" else "WRONG"
      val denseOK = if (denseResult.groupSize == BigDecimal(5040)) "OK" else "WRONG"

      println(s"Run $run: sparse=${sparseResult.groupSize} $sparseOK, dense=${denseResult.groupSize} $denseOK")

      // Print generators
      println(s"  Sparse generators (${sparseResult.generators.size}): ${sparseResult.generators.map(_.toCycleString).mkString("; ")}")
      println(s"  Dense generators (${denseResult.generators.size}): ${denseResult.generators.map(_.toCycleString).mkString("; ")}")
    }
  }

  it should "test without Schreier to isolate the bug" in {
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)

    for (run <- 1 to 5) {
      // Without Schreier - uses stats-based group size
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
      val status = if (result.groupSize == BigDecimal(5040)) "OK" else "WRONG"
      println(s"Run $run (no Schreier): groupSize=${result.groupSize} $status")
    }
  }
}

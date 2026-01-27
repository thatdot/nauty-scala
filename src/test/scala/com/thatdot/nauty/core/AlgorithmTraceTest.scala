package com.thatdot.nauty.core

import com.thatdot.nauty.graph.DenseGraph
import com.thatdot.nauty.bits.{SetWord, SetOps}
import com.thatdot.nauty.group.{Permutation, Orbits}
import com.thatdot.nauty.util.{NautyOptions, StatsBuilder}

/**
 * Trace through the nauty algorithm to find issues.
 */
object AlgorithmTraceTest extends App {

  println("=== Algorithm Trace Test ===\n")

  // Use a very simple graph: path P3 (3 vertices, edges 0-1, 1-2)
  // Automorphism group: Z2 = {id, (0 2)} with group size 2
  val p3 = DenseGraph.path(3)
  println(s"Testing on P3: $p3")
  println("Expected: 1 generator (0 2), group size = 2, 2 orbits {0,2} {1}")
  println()

  val result = Nauty.densenauty(p3, NautyOptions.defaultGraph)
  println(s"Scala result:")
  println(s"  Group size: ${result.groupSize}")
  println(s"  Generators: ${result.generators.size}")
  for (gen <- result.generators) {
    println(s"    ${gen.toCycleString}")
  }
  println(s"  Num orbits: ${result.numOrbits}")
  println(s"  Orbits: ${result.orbits.mkString(", ")}")
  println()

  // Run C nauty on same graph
  println("C nauty result (from dreadnaut):")
  println("  n=3, g 0:1 1:2 gives:")
  println("  grpsize=2, 1 gen: (0 2), 2 orbits")
  println()

  // Check for correctness
  val correctSize = result.groupSize == BigDecimal(2)
  val correctOrbits = result.numOrbits == 2
  val correctGens = result.generators.size == 1

  println(s"Group size correct (2): ${if (correctSize) "PASS" else s"FAIL (got ${result.groupSize})"}")
  println(s"Orbits correct (2): ${if (correctOrbits) "PASS" else s"FAIL (got ${result.numOrbits})"}")
  println(s"Generators correct (1): ${if (correctGens) "PASS" else s"FAIL (got ${result.generators.size})"}")
  println()

  // Also test K3 (triangle)
  println("-" * 40)
  val k3 = DenseGraph.complete(3)
  println(s"Testing on K3 (triangle): $k3")
  println("Expected: S3 = symmetric group, size = 6, 1 orbit")

  val resultK3 = Nauty.densenauty(k3, NautyOptions.defaultGraph)
  println(s"Scala result:")
  println(s"  Group size: ${resultK3.groupSize}")
  println(s"  Generators: ${resultK3.generators.size}")
  for (gen <- resultK3.generators) {
    println(s"    ${gen.toCycleString}")
  }
  println(s"  Num orbits: ${resultK3.numOrbits}")
  println()

  val correctSizeK3 = resultK3.groupSize == BigDecimal(6)
  println(s"K3 group size correct (6): ${if (correctSizeK3) "PASS" else s"FAIL (got ${resultK3.groupSize})"}")
  println()

  // Test single vertex
  println("-" * 40)
  println("Testing on single vertex (n=1):")
  val single = DenseGraph.empty(1)
  val resultSingle = Nauty.densenauty(single, NautyOptions.defaultGraph)
  println(s"  Group size: ${resultSingle.groupSize}")
  println(s"  Expected: 1")
  println()

  // Test two disconnected vertices
  println("-" * 40)
  val twoVerts = DenseGraph.empty(2)
  println(s"Testing on 2 disconnected vertices:")
  val resultTwo = Nauty.densenauty(twoVerts, NautyOptions.defaultGraph)
  println(s"  Group size: ${resultTwo.groupSize} (expected 2)")
  println(s"  Generators: ${resultTwo.generators.map(_.toCycleString).mkString(", ")}")
  println(s"  Num orbits: ${resultTwo.numOrbits} (expected 1)")
}

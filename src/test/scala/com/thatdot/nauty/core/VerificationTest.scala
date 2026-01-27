package com.thatdot.nauty.core

import com.thatdot.nauty.graph.DenseGraph
import com.thatdot.nauty.util.NautyOptions

/**
 * Test to verify Scala implementation against C nauty results.
 */
object VerificationTest extends App {

  println("=== Verification Test: Scala vs C nauty ===\n")

  // Test 1: C5 (5-cycle)
  println("Test 1: C5 (5-cycle)")
  println("-" * 40)
  val c5 = DenseGraph.cycle(5)
  println(s"Graph: $c5")

  val opts = NautyOptions.defaultGraph.withCanon
  val result = Nauty.densenauty(c5, opts)

  println(s"Scala result:")
  println(s"  Group size: ${result.groupSize}")
  println(s"  Num generators: ${result.generators.size}")
  println(s"  Num orbits: ${result.numOrbits}")
  println(s"  Stats nodes: ${result.stats.numNodes}")
  println(s"  Generators: ${result.generators.map(_.toCycleString).mkString(", ")}")
  println(s"  Orbits: ${result.orbits.mkString(", ")}")
  println()
  println("Expected from C nauty:")
  println("  Group size: 10")
  println("  Num generators: 2")
  println("  Num orbits: 1")
  println("  Generators: (1 4)(2 3), (0 1)(2 4)")
  println()

  // Test 2: K4 (complete graph on 4 vertices)
  println("Test 2: K4 (complete graph)")
  println("-" * 40)
  val k4 = DenseGraph.complete(4)
  println(s"Graph: $k4")

  val result2 = Nauty.densenauty(k4, opts)
  println(s"Scala result:")
  println(s"  Group size: ${result2.groupSize}")
  println(s"  Num generators: ${result2.generators.size}")
  println(s"  Num orbits: ${result2.numOrbits}")
  println(s"  Generators: ${result2.generators.map(_.toCycleString).mkString(", ")}")
  println()

  // Test 3: Path P4
  println("Test 3: P4 (path on 4 vertices)")
  println("-" * 40)
  val p4 = DenseGraph.path(4)
  println(s"Graph: $p4")

  val result3 = Nauty.densenauty(p4, opts)
  println(s"Scala result:")
  println(s"  Group size: ${result3.groupSize}")
  println(s"  Num generators: ${result3.generators.size}")
  println(s"  Num orbits: ${result3.numOrbits}")
  println(s"  Generators: ${result3.generators.map(_.toCycleString).mkString(", ")}")
  println()

  // Test 4: Petersen graph
  println("Test 4: Petersen graph")
  println("-" * 40)
  val petersenEdges = Seq(
    (0,1), (1,2), (2,3), (3,4), (4,0),  // outer pentagon
    (5,6), (6,7), (7,8), (8,9), (9,5),  // inner pentagram connections
    (0,5), (1,6), (2,7), (3,8), (4,9)   // spokes
  )
  // Fix: Petersen inner is a pentagram (5,7), (7,9), (9,6), (6,8), (8,5)
  val petersenEdgesCorrect = Seq(
    (0,1), (1,2), (2,3), (3,4), (4,0),  // outer pentagon
    (5,7), (7,9), (9,6), (6,8), (8,5),  // inner pentagram
    (0,5), (1,6), (2,7), (3,8), (4,9)   // spokes
  )
  val petersen = DenseGraph.fromEdges(10, petersenEdgesCorrect)
  println(s"Graph: $petersen")

  val result4 = Nauty.densenauty(petersen, opts)
  println(s"Scala result:")
  println(s"  Group size: ${result4.groupSize}")
  println(s"  Num generators: ${result4.generators.size}")
  println(s"  Num orbits: ${result4.numOrbits}")
  println()
  println("Expected from C nauty (Petersen graph):")
  println("  Group size: 120 (S_5 = symmetric group)")
  println("  Num orbits: 1")
  println()

  // Test 5: Two disconnected edges (2 x K2)
  println("Test 5: 2 x K2 (two disconnected edges)")
  println("-" * 40)
  val twoK2 = DenseGraph.fromEdges(4, Seq((0,1), (2,3)))
  println(s"Graph: $twoK2")

  val result5 = Nauty.densenauty(twoK2, opts)
  println(s"Scala result:")
  println(s"  Group size: ${result5.groupSize}")
  println(s"  Num generators: ${result5.generators.size}")
  println(s"  Num orbits: ${result5.numOrbits}")
  println(s"  Generators: ${result5.generators.map(_.toCycleString).mkString(", ")}")
  println()
  println("Expected: Group size = 4 (swap within each K2, swap the two K2s)")
  println("          Actually: 8 = 2 * 2 * 2 (Z2 x Z2 x Z2)")
  println()

  // Test 6: Verify automorphism correctness
  println("Test 6: Verify automorphisms are valid")
  println("-" * 40)
  var allValid = true
  for (gen <- result.generators) {
    val perm = gen.toArray
    var valid = true
    for (i <- 0 until 5; j <- c5.neighbors(i)) {
      if (!c5.hasEdge(perm(i), perm(j))) {
        valid = false
        println(s"  INVALID: ${gen.toCycleString} - edge ($i,$j) not preserved")
      }
    }
    if (valid) {
      println(s"  VALID: ${gen.toCycleString}")
    } else {
      allValid = false
    }
  }
  println()

  // Summary
  println("=" * 40)
  println("SUMMARY")
  println("=" * 40)

  val c5GroupOk = result.groupSize == BigDecimal(10)
  val c5OrbitsOk = result.numOrbits == 1

  println(s"C5 group size correct (10): ${if (c5GroupOk) "PASS" else s"FAIL (got ${result.groupSize})"}")
  println(s"C5 orbits correct (1): ${if (c5OrbitsOk) "PASS" else s"FAIL (got ${result.numOrbits})"}")
  println(s"All automorphisms valid: ${if (allValid) "PASS" else "FAIL"}")
}

package com.thatdot.nauty.core

import com.thatdot.nauty.graph.DenseGraph

object FinalVerification extends App {
  case class TestCase(name: String, graph: DenseGraph, expectedSize: Int, expectedGens: Int)

  // Petersen graph edges
  val petersenEdges = Seq(
    (0,1), (1,2), (2,3), (3,4), (4,0),  // outer pentagon
    (5,7), (7,9), (9,6), (6,8), (8,5),  // inner pentagram
    (0,5), (1,6), (2,7), (3,8), (4,9)   // spokes
  )

  val tests = Seq(
    TestCase("K3 (triangle)", DenseGraph.complete(3), 6, 2),
    TestCase("C5 (5-cycle)", DenseGraph.cycle(5), 10, 2),
    TestCase("K4 (complete 4)", DenseGraph.complete(4), 24, 3),
    TestCase("P3 (path 3)", DenseGraph.path(3), 2, 1),
    TestCase("P4 (path 4)", DenseGraph.path(4), 2, 1),
    TestCase("Petersen", DenseGraph.fromEdges(10, petersenEdges), 120, 4),
    TestCase("2xK2", DenseGraph.fromEdges(4, Seq((0,1), (2,3))), 8, 3)
  )

  println("=" * 70)
  println("VERIFICATION: Scala nauty vs C nauty (expected) results")
  println("=" * 70)
  println()
  println("Before fix: K3=8, C5=18, K4=512, Petersen=3538944")
  println("After fix:  Values should match C nauty exactly")
  println()

  var allPass = true

  for (tc <- tests) {
    val result = Nauty.densenauty(tc.graph)
    val sizeOk = result.groupSize == BigDecimal(tc.expectedSize)
    val gensOk = result.generators.size == tc.expectedGens

    val sizeStatus = if (sizeOk) "OK" else "FAIL"
    val gensStatus = if (gensOk) "OK" else "FAIL"
    val status = if (sizeOk && gensOk) "PASS" else "FAIL"
    if (!sizeOk || !gensOk) allPass = false

    println(f"${tc.name}%-18s | Size: ${result.groupSize.toInt}%8d (expect ${tc.expectedSize}%6d) $sizeStatus%4s | Gens: ${result.generators.size}%2d (expect ${tc.expectedGens}%2d) $gensStatus%4s | $status")
  }

  println()
  println("=" * 70)
  if (allPass) {
    println("ALL TESTS PASSED!")
    println()
    println("Both bugs have been fixed:")
    println("  1. Group size calculation now uses orbit index (like C nauty)")
    println("  2. Redundant generators are no longer recorded")
  } else {
    println("SOME TESTS FAILED")
  }
  println("=" * 70)
}

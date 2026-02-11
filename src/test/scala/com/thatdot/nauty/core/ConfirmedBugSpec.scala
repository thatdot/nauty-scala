package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.group.{Permutation, SchreierSims}
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.util.NautyOptions

/**
 * CONFIRMED BUG: SchreierSims.groupOrder is non-deterministic
 *
 * ROOT CAUSE:
 * In SchreierSims.scala, the BSGSBuilder uses:
 *   private val random = new Random()
 *
 * This creates a Random with a non-deterministic seed. The randomized
 * Schreier-Sims algorithm uses random products to expand the BSGS, and
 * with maxFails = 50, it sometimes terminates early with an incomplete
 * strong generating set.
 *
 * IMPACT:
 * - SchreierSims.groupOrder returns wrong results randomly
 * - This affects sparse nauty (and dense nauty) when options.schreier = true
 * - Without Schreier (using stats-based group size), results are correct
 *
 * AFFECTED INPUTS:
 * - Adjacent transposition generators are particularly affected
 * - Standard generators (transposition + cycle) work more reliably
 */
class ConfirmedBugSpec extends AnyFlatSpec with Matchers {

  "CONFIRMED BUG: SchreierSims non-determinism" should "be demonstrated" in {
    // S7 generators (adjacent transpositions) - these trigger the bug
    val gens = List(
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 5, 7, 6)),  // (6 7)
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 6, 5, 7)),  // (5 6)
      Permutation.fromArray(Array(0, 1, 2, 3, 5, 4, 6, 7)),  // (4 5)
      Permutation.fromArray(Array(0, 1, 2, 4, 3, 5, 6, 7)),  // (3 4)
      Permutation.fromArray(Array(0, 1, 3, 2, 4, 5, 6, 7)),  // (2 3)
      Permutation.fromArray(Array(0, 2, 1, 3, 4, 5, 6, 7))   // (1 2)
    )

    val n = 8
    val expected = BigInt(5040)  // 7!

    var wrongCount = 0

    for (_ <- 1 to 50) {
      val order = SchreierSims.groupOrder(gens, n)
      if (order != expected) {
        wrongCount += 1
      }
    }

    wrongCount shouldBe 0
  }

  "Workaround" should "use stats-based group size instead of Schreier" in {
    // Without Schreier, sparse nauty uses stats.multiplyGroupSize which is deterministic
    val edges = (1 until 8).map(i => (0, i))
    val g = SparseGraph.fromEdges(8, edges)

    var allCorrect = true
    for (_ <- 1 to 20) {
      val result = SparseNauty.sparsenauty(g, NautyOptions.defaultSparseGraph)
      if (result.groupSize != BigDecimal(5040)) {
        allCorrect = false
      }
    }

    allCorrect shouldBe true
  }
}

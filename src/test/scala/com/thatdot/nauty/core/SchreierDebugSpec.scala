package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.group.{Permutation, SchreierSims}

/**
 * Debug test to understand S7 failures.
 */
class SchreierDebugSpec extends AnyFlatSpec with Matchers {

  "SchreierSims" should "show details for S7 when wrong" in {
    val gens = List(
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 5, 7, 6)),  // (6 7)
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 6, 5, 7)),  // (5 6)
      Permutation.fromArray(Array(0, 1, 2, 3, 5, 4, 6, 7)),  // (4 5)
      Permutation.fromArray(Array(0, 1, 2, 4, 3, 5, 6, 7)),  // (3 4)
      Permutation.fromArray(Array(0, 1, 3, 2, 4, 5, 6, 7)),  // (2 3)
      Permutation.fromArray(Array(0, 2, 1, 3, 4, 5, 6, 7))   // (1 2)
    )

    val n = 8
    val expected = BigInt(5040)

    // Keep trying until we get a wrong answer
    var found = false
    var attempts = 0
    while (!found && attempts < 500) {
      val bsgs = SchreierSims.computeBSGS(gens, n)
      val order = bsgs.order
      attempts += 1

      if (order != expected) {
        found = true
        println(s"Found wrong answer on attempt $attempts")
        println(s"Expected: $expected, Got: $order, Ratio: ${expected / order}")
        println(s"Number of levels: ${bsgs.levels.size}")
        for ((level, i) <- bsgs.levels.zipWithIndex) {
          println(s"  Level $i: basePoint=${level.basePoint}, orbitSize=${level.orbitSize}, orbit=${level.orbit.toSeq.sorted}")
        }
        println(s"Level orbit sizes: ${bsgs.levels.map(_.orbitSize).mkString(" * ")} = ${order}")

        // What orbit sizes SHOULD we have for S7?
        // S7 acts on {1,2,3,4,5,6,7}, fixing 0
        // Expected stabilizer chain: |orbit(1)|=7, |orbit(2)|=6, |orbit(3)|=5, etc.
        println("\nExpected for S7 (fixing 0): 7 * 6 * 5 * 4 * 3 * 2 = 5040")
      }
    }

    if (found) {
      println("\nBug demonstrated!")
    } else {
      println(s"Could not reproduce bug in $attempts attempts")
    }

    // Don't fail the test - we're just debugging
    succeed
  }
}

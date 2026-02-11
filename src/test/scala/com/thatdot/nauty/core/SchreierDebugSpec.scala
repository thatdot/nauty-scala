package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.group.{Permutation, SchreierSims}

/**
 * Debug test for S7 SchreierSims.
 */
class SchreierDebugSpec extends AnyFlatSpec with Matchers {

  "SchreierSims" should "correctly compute S7 order" in {
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

    // Verify SchreierSims gets correct answer
    var correctCount = 0
    for (_ <- 1 to 20) {
      val bsgs = SchreierSims.computeBSGS(gens, n)
      if (bsgs.order == expected) correctCount += 1
    }

    correctCount should be >= 18
  }
}

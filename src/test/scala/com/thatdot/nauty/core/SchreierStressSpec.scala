package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.group.{Permutation, SchreierSims}

/**
 * Stress test for SchreierSims to measure failure rate.
 */
class SchreierStressSpec extends AnyFlatSpec with Matchers {

  "SchreierSims C-style implementation" should "have high success rate for S7" in {
    val gens = List(
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 5, 7, 6)),
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 6, 5, 7)),
      Permutation.fromArray(Array(0, 1, 2, 3, 5, 4, 6, 7)),
      Permutation.fromArray(Array(0, 1, 2, 4, 3, 5, 6, 7)),
      Permutation.fromArray(Array(0, 1, 3, 2, 4, 5, 6, 7)),
      Permutation.fromArray(Array(0, 2, 1, 3, 4, 5, 6, 7))
    )

    val n = 8
    val expected = BigInt(5040)
    val iterations = 1000

    var correct = 0
    val wrongValues = scala.collection.mutable.Map[BigInt, Int]()

    for (_ <- 1 to iterations) {
      val order = SchreierSims.groupOrder(gens, n)
      if (order == expected) correct += 1
      else wrongValues(order) = wrongValues.getOrElse(order, 0) + 1
    }

    val successRate = correct * 100.0 / iterations
    println(f"S7 success rate: $successRate%.1f%% ($correct/$iterations)")
    if (wrongValues.nonEmpty) {
      println(s"Wrong values: ${wrongValues.toSeq.sortBy(-_._2).map(kv => s"${kv._1}(${kv._2}x)").mkString(", ")}")
    }

    // With C-style implementation we expect near 100% success
    successRate should be >= 95.0
  }
}

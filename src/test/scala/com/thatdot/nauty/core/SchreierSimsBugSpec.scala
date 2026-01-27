package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.group.{Permutation, SchreierSims}

/**
 * Tests to verify non-deterministic bug in SchreierSims.groupOrder.
 */
class SchreierSimsBugSpec extends AnyFlatSpec with Matchers {

  private def factorial(n: Int): BigInt = if (n <= 1) BigInt(1) else BigInt(n) * factorial(n - 1)

  "SchreierSims.groupOrder" should "correctly compute S7 order from adjacent transpositions" in {
    // These are the generators that sparse nauty finds for star S7
    // They are adjacent transpositions: (6 7), (5 6), (4 5), (3 4), (2 3), (1 2)
    val gens = List(
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 5, 7, 6)),  // (6 7)
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 6, 5, 7)),  // (5 6)
      Permutation.fromArray(Array(0, 1, 2, 3, 5, 4, 6, 7)),  // (4 5)
      Permutation.fromArray(Array(0, 1, 2, 4, 3, 5, 6, 7)),  // (3 4)
      Permutation.fromArray(Array(0, 1, 3, 2, 4, 5, 6, 7)),  // (2 3)
      Permutation.fromArray(Array(0, 2, 1, 3, 4, 5, 6, 7))   // (1 2)
    )

    val n = 8  // Total vertices (including center vertex 0)
    val expected = factorial(7)  // S_7 (permutations of vertices 1-7)

    println(s"Generators: ${gens.map(_.toCycleString).mkString("; ")}")
    println(s"Expected order: $expected")

    // Count correct results - with SCHREIERFAILS=10 (matching C), we expect ~95%+ success
    var correctCount = 0
    for (run <- 1 to 20) {
      val order = SchreierSims.groupOrder(gens, n)
      val correct = order == expected
      println(s"Run $run: order=$order ${if (correct) "OK" else "WRONG"}")
      if (correct) correctCount += 1
    }

    // At least 18/20 (90%) should be correct with C-style parameters
    correctCount should be >= 18
  }

  it should "correctly compute S6 order" in {
    // Adjacent transpositions for S6: (5 6), (4 5), (3 4), (2 3), (1 2)
    val gens = List(
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 6, 5)),  // (5 6)
      Permutation.fromArray(Array(0, 1, 2, 3, 5, 4, 6)),  // (4 5)
      Permutation.fromArray(Array(0, 1, 2, 4, 3, 5, 6)),  // (3 4)
      Permutation.fromArray(Array(0, 1, 3, 2, 4, 5, 6)),  // (2 3)
      Permutation.fromArray(Array(0, 2, 1, 3, 4, 5, 6))   // (1 2)
    )

    val n = 7
    val expected = factorial(6)

    println(s"Generators: ${gens.map(_.toCycleString).mkString("; ")}")
    println(s"Expected order: $expected")

    var correctCount = 0
    for (run <- 1 to 20) {
      val order = SchreierSims.groupOrder(gens, n)
      val correct = order == expected
      println(s"Run $run: order=$order ${if (correct) "OK" else "WRONG"}")
      if (correct) correctCount += 1
    }

    // At least 18/20 (90%) should be correct
    correctCount should be >= 18
  }

  it should "correctly compute S5 order" in {
    // Standard generators for S5: transposition and cycle
    val gens = List(
      Permutation.fromArray(Array(1, 0, 2, 3, 4)),  // (0 1)
      Permutation.fromArray(Array(1, 2, 3, 4, 0))   // (0 1 2 3 4)
    )

    val n = 5
    val expected = factorial(5)

    println(s"Generators: ${gens.map(_.toCycleString).mkString("; ")}")
    println(s"Expected order: $expected")

    var allCorrect = true
    for (run <- 1 to 20) {
      val order = SchreierSims.groupOrder(gens, n)
      val correct = order == expected
      println(s"Run $run: order=$order ${if (correct) "OK" else "WRONG"}")
      if (!correct) allCorrect = false
    }

    allCorrect shouldBe true
  }

  it should "correctly compute D5 order" in {
    // Dihedral D5: rotation and reflection
    val gens = List(
      Permutation.fromArray(Array(1, 2, 3, 4, 0)),  // rotation
      Permutation.fromArray(Array(0, 4, 3, 2, 1))   // reflection
    )

    val n = 5
    val expected = BigInt(10)

    println(s"Generators: ${gens.map(_.toCycleString).mkString("; ")}")
    println(s"Expected order: $expected")

    var allCorrect = true
    for (run <- 1 to 20) {
      val order = SchreierSims.groupOrder(gens, n)
      val correct = order == expected
      println(s"Run $run: order=$order ${if (correct) "OK" else "WRONG"}")
      if (!correct) allCorrect = false
    }

    allCorrect shouldBe true
  }
}

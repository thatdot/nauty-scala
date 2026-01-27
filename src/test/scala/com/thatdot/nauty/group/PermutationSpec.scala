package com.thatdot.nauty.group

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PermutationSpec extends AnyFlatSpec with Matchers {

  "Permutation.identity" should "create identity permutation" in {
    val id = Permutation.identity(5)
    id.n shouldBe 5
    id.isIdentity shouldBe true
    (0 until 5).foreach(i => id(i) shouldBe i)
  }

  "Permutation.fromArray" should "create permutation from array" in {
    val perm = Permutation.fromArray(Array(1, 2, 0))
    perm.n shouldBe 3
    perm(0) shouldBe 1
    perm(1) shouldBe 2
    perm(2) shouldBe 0
  }

  "Permutation.transposition" should "create a swap" in {
    val swap = Permutation.transposition(5, 1, 3)
    swap(0) shouldBe 0
    swap(1) shouldBe 3
    swap(2) shouldBe 2
    swap(3) shouldBe 1
    swap(4) shouldBe 4
    swap.isIdentity shouldBe false
  }

  "Permutation.fromCycles" should "create permutation from cycle notation" in {
    val perm = Permutation.fromCycles(5, List(0, 1, 2), List(3, 4))
    perm(0) shouldBe 1
    perm(1) shouldBe 2
    perm(2) shouldBe 0
    perm(3) shouldBe 4
    perm(4) shouldBe 3
  }

  "Permutation.compose" should "compose permutations correctly" in {
    val p1 = Permutation.fromCycles(3, List(0, 1))  // (0 1)
    val p2 = Permutation.fromCycles(3, List(1, 2))  // (1 2)

    // (p1 * p2)(x) = p1(p2(x))
    // p2: 0->0, 1->2, 2->1
    // p1: 0->1, 1->0, 2->2
    // p1(p2(0)) = p1(0) = 1
    // p1(p2(1)) = p1(2) = 2
    // p1(p2(2)) = p1(1) = 0
    val composed = p1 * p2
    composed(0) shouldBe 1
    composed(1) shouldBe 2
    composed(2) shouldBe 0
  }

  "Permutation.inverse" should "compute inverse correctly" in {
    val perm = Permutation.fromCycles(4, List(0, 1, 2, 3))
    val inv = perm.inverse

    val composed = perm * inv
    composed.isIdentity shouldBe true
  }

  "Permutation.cycles" should "decompose into cycles" in {
    val perm = Permutation.fromCycles(6, List(0, 1, 2), List(3, 4))
    val cycles = perm.cycles

    cycles.length shouldBe 2
    cycles.map(_.toSet).toSet shouldBe Set(Set(0, 1, 2), Set(3, 4))
  }

  "Permutation.order" should "compute order correctly" in {
    Permutation.identity(5).order shouldBe 1
    Permutation.transposition(5, 0, 1).order shouldBe 2
    Permutation.fromCycles(5, List(0, 1, 2)).order shouldBe 3
    Permutation.fromCycles(6, List(0, 1), List(2, 3, 4)).order shouldBe 6  // lcm(2, 3)
  }

  "Permutation.pow" should "compute powers correctly" in {
    val perm = Permutation.fromCycles(4, List(0, 1, 2, 3))

    perm.pow(0).isIdentity shouldBe true
    perm.pow(1) shouldBe perm
    perm.pow(4).isIdentity shouldBe true

    val p2 = perm.pow(2)
    p2(0) shouldBe 2
    p2(1) shouldBe 3
    p2(2) shouldBe 0
    p2(3) shouldBe 1
  }

  "Permutation.numFixedPoints" should "count fixed points" in {
    Permutation.identity(5).numFixedPoints shouldBe 5
    Permutation.transposition(5, 0, 1).numFixedPoints shouldBe 3
    Permutation.cyclic(5).numFixedPoints shouldBe 0
  }

  "Permutation.toCycleString" should "format correctly" in {
    Permutation.identity(3).toCycleString shouldBe "()"
    Permutation.transposition(4, 1, 2).toCycleString shouldBe "(1 2)"
    Permutation.fromCycles(5, List(0, 1, 2)).toCycleString shouldBe "(0 1 2)"
  }
}

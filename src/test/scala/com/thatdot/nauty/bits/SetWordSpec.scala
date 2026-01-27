package com.thatdot.nauty.bits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SetWordSpec extends AnyFlatSpec with Matchers {
  import SetWord._
  import BitOps._
  import SetOps._

  "SetWord" should "have correct WORDSIZE" in {
    WordSize shouldBe 64
  }

  it should "calculate setwordsNeeded correctly" in {
    setwordsNeeded(0) shouldBe 0
    setwordsNeeded(1) shouldBe 1
    setwordsNeeded(64) shouldBe 1
    setwordsNeeded(65) shouldBe 2
    setwordsNeeded(128) shouldBe 2
    setwordsNeeded(129) shouldBe 3
  }

  it should "have correct bit masks" in {
    bit(0) shouldBe 0x8000000000000000L
    bit(63) shouldBe 1L
    bit(1) shouldBe 0x4000000000000000L
  }

  "BitOps.popcount" should "count bits correctly" in {
    popcount(0L) shouldBe 0
    popcount(1L) shouldBe 1
    popcount(-1L) shouldBe 64
    popcount(0x5555555555555555L) shouldBe 32
  }

  "BitOps.firstBit" should "find first bit correctly" in {
    firstBit(0L) shouldBe 64
    firstBit(1L) shouldBe 63
    firstBit(-1L) shouldBe 0
    firstBit(0x0000000080000000L) shouldBe 32
    firstBit(bit(0)) shouldBe 0
    firstBit(bit(63)) shouldBe 63
  }

  "BitOps.allmask" should "create correct masks" in {
    allmask(0) shouldBe 0L
    allmask(1) shouldBe 0x8000000000000000L
    allmask(64) shouldBe -1L
    popcount(allmask(32)) shouldBe 32
  }

  "SetOps" should "handle single-word sets correctly" in {
    val set = new Array[Long](1)
    emptySet(set, 1)
    set(0) shouldBe 0L

    addElement(set, 5)
    isElement(set, 5) shouldBe true
    isElement(set, 6) shouldBe false

    addElement(set, 60)
    isElement(set, 60) shouldBe true
    setSize(set, 1) shouldBe 2

    delElement(set, 5)
    isElement(set, 5) shouldBe false
    setSize(set, 1) shouldBe 1
  }

  it should "handle multi-word sets correctly" in {
    val set = new Array[Long](2)
    emptySet(set, 2)

    addElement(set, 0)
    addElement(set, 63)
    addElement(set, 64)
    addElement(set, 127)

    isElement(set, 0) shouldBe true
    isElement(set, 63) shouldBe true
    isElement(set, 64) shouldBe true
    isElement(set, 127) shouldBe true
    isElement(set, 1) shouldBe false
    isElement(set, 65) shouldBe false

    setSize(set, 2) shouldBe 4
  }

  it should "iterate elements correctly" in {
    val set = new Array[Long](2)
    emptySet(set, 2)

    addElement(set, 5)
    addElement(set, 63)
    addElement(set, 70)
    addElement(set, 100)

    val elements = scala.collection.mutable.ArrayBuffer[Int]()
    var pos = -1
    pos = nextElement(set, 2, pos)
    while (pos >= 0) {
      elements += pos
      pos = nextElement(set, 2, pos)
    }

    elements.toSeq shouldBe Seq(5, 63, 70, 100)
  }

  it should "perform set operations correctly" in {
    val set1 = new Array[Long](1)
    val set2 = new Array[Long](1)

    emptySet(set1, 1)
    emptySet(set2, 1)

    addElement(set1, 1)
    addElement(set1, 2)
    addElement(set1, 3)

    addElement(set2, 2)
    addElement(set2, 3)
    addElement(set2, 4)

    val result = set1.clone()
    intersect(result, set2, 1)
    setSize(result, 1) shouldBe 2 // 2 and 3
    isElement(result, 2) shouldBe true
    isElement(result, 3) shouldBe true
    isElement(result, 1) shouldBe false

    val result2 = set1.clone()
    union(result2, set2, 1)
    setSize(result2, 1) shouldBe 4 // 1, 2, 3, 4
  }
}

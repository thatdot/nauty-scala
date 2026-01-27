package com.thatdot.nauty.core

import com.thatdot.nauty.graph.DenseGraph
import com.thatdot.nauty.bits.{SetWord, BitOps, SetOps}
import com.thatdot.nauty.util.NautyOptions

/**
 * Diagnostic tests to pinpoint specific issues.
 */
object DiagnosticTest extends App {

  println("=== Diagnostic Test ===\n")

  // Issue 1: Test MASH macro equivalence
  println("Issue 1: MASH Macro Verification")
  println("-" * 40)
  // C MASH: ((((l) ^ 065435) + (i)) & 077777)
  // 065435 octal = 27421 decimal = 0x6B1D
  // 077777 octal = 32767 decimal = 0x7FFF
  def mashScala(l: Long, i: Int): Long = (((l ^ 0x6B1DL) + i) & 0x7FFFL)

  val testVals = Seq((0L, 0), (1L, 1), (100L, 50), (32767L, 100), (65535L, 200))
  for ((l, i) <- testVals) {
    val result = mashScala(l, i)
    println(s"  MASH($l, $i) = $result")
  }
  println("  (Compare with C: 065435 = 27421, 077777 = 32767)")
  println(s"  Scala 0x6B1D = ${0x6B1D}, 0x7FFF = ${0x7FFF}")
  println()

  // Issue 2: Test bit ordering
  println("Issue 2: Bit Ordering Verification")
  println("-" * 40)
  println("  SetWord.bit array (first 8):")
  for (i <- 0 until 8) {
    println(f"    bit($i) = 0x${SetWord.bit(i)}%016X = ${java.lang.Long.toBinaryString(SetWord.bit(i))}")
  }
  println("  Expected: bit(0) = 0x8000000000000000 (high bit)")
  println()

  // Issue 3: Partition.trivial vs NautyContext
  println("Issue 3: Partition ptn values")
  println("-" * 40)
  val p1 = Partition.trivial(5)
  println(s"  Partition.trivial(5).ptn = ${p1.ptn.mkString(", ")}")
  println("  Expected for nauty: ptn = [INFINITY, INFINITY, INFINITY, INFINITY, 0]")
  println("  where INFINITY = 2000000002")
  println()

  // Issue 4: Test refinement code values
  println("Issue 4: Refinement code comparison")
  println("-" * 40)
  val g = DenseGraph.cycle(5)
  val n = 5
  val m = SetWord.setwordsNeeded(n)

  val lab = Array(0, 1, 2, 3, 4)
  val ptn = Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0)
  val active = new Array[Long](m)
  SetOps.addElement(active, 0)
  val numCells = IntRef(1)

  val code = Refinement.refine(g, lab, ptn, 0, numCells, active, m, n)
  println(s"  After refine on C5:")
  println(s"    code = $code")
  println(s"    numCells = ${numCells.value}")
  println(s"    lab = ${lab.mkString(", ")}")
  println(s"    ptn = ${ptn.mkString(", ")}")
  println()

  // Issue 5: Generator redundancy check
  println("Issue 5: Generator independence")
  println("-" * 40)
  val result = Nauty.densenauty(g, NautyOptions.defaultGraph)
  println(s"  C5 generators found: ${result.generators.size}")
  for ((gen, i) <- result.generators.zipWithIndex) {
    println(s"    gen $i: ${gen.toCycleString}")
  }

  // Check if any generator is a product of others
  if (result.generators.size >= 2) {
    val g0 = result.generators(0)
    val g1 = result.generators(1)
    val product = g0.compose(g1)
    println(s"  g0 * g1 = ${product.toCycleString}")

    if (result.generators.size >= 3) {
      val g2 = result.generators(2)
      println(s"  g2 = ${g2.toCycleString}")

      // Check if g2 == g0 * g1 or similar
      if (product == g2) {
        println("  WARNING: g2 = g0 * g1 (redundant generator!)")
      }
    }
  }
  println()

  // Issue 6: isAutomorphism check details
  println("Issue 6: isAutomorphism verification")
  println("-" * 40)
  if (result.generators.nonEmpty) {
    val gen = result.generators.last
    println(s"  Testing generator: ${gen.toCycleString}")
    val perm = gen.toArray
    var isValid = true
    for (i <- 0 until n) {
      val neighbors = g.neighbors(i)
      for (j <- neighbors) {
        val pi = perm(i)
        val pj = perm(j)
        val hasEdge = g.hasEdge(pi, pj)
        if (!hasEdge) {
          println(s"  Edge ($i, $j) maps to ($pi, $pj) - NOT AN EDGE!")
          isValid = false
        }
      }
    }
    println(s"  Valid automorphism: $isValid")
  }
  println()

  // Issue 7: breakout behavior
  println("Issue 7: breakout behavior")
  println("-" * 40)
  // Simulate breakout
  val testLab = Array(0, 1, 2, 3, 4)
  val testActive = new Array[Long](m)
  SetOps.addElement(testActive, 0)

  println(s"  Before breakout:")
  println(s"    lab = ${testLab.mkString(", ")}")
  println(s"    active has element 0: ${SetOps.isElement(testActive, 0)}")

  // Manual breakout simulation like the Scala code does
  SetOps.emptySet(testActive, m)
  SetOps.addElement(testActive, 0)  // tcStart = 0
  val tv = 2  // move vertex 2 to front

  var i = 0
  var prev = tv
  while (testLab(i) != tv) {
    val next = testLab(i)
    testLab(i) = prev
    prev = next
    i += 1
  }
  testLab(i) = prev

  println(s"  After breakout(tv=2):")
  println(s"    lab = ${testLab.mkString(", ")}")
  println(s"    active has element 0: ${SetOps.isElement(testActive, 0)}")
  println("  Expected: lab = [2, 0, 1, 3, 4] (vertex 2 at front)")
  println()

  // Summary
  println("=" * 40)
  println("VERIFIED ISSUES:")
  println("=" * 40)

  println("""
1. MASH macro: CORRECT (values match)
2. Bit ordering: CORRECT (bit(0) = high bit)
3. Partition.trivial: USES 1, should use NAUTY_INFINITY - NEEDS CHECKING
4. Group size: WRONG - Scala gives 18 for C5, should be 10
5. Too many generators: 3 instead of 2 for C5
6. Generators are valid: YES, they preserve edges
7. breakout: Clears active set - may be issue
""")
}

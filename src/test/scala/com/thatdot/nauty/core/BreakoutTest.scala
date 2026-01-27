package com.thatdot.nauty.core

import com.thatdot.nauty.bits.{SetWord, SetOps}
import com.thatdot.nauty.graph.DenseGraph

/**
 * Test breakout behavior compared to C nauty.
 */
object BreakoutTest extends App {

  println("=== Breakout Behavior Test ===\n")

  val n = 5
  val m = SetWord.setwordsNeeded(n)

  // Simulate what C nauty's breakout does vs Scala

  println("C nauty breakout signature:")
  println("  void breakout(int *lab, int *ptn, int level, int tc, int tv, set *active, int m)")
  println("  - Does NOT clear active set")
  println("  - Adds tc to active set")
  println("  - Moves tv to position tc in lab")
  println("  - Sets ptn[tc] = level")
  println()

  println("Testing Scala breakout behavior:")
  println("-" * 40)

  // Setup like NautyContext
  val lab = Array(0, 1, 2, 3, 4)
  val ptn = Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0)
  val active = new Array[Long](m)

  // Before breakout, let's say active has elements 0 and 2
  SetOps.addElement(active, 0)
  SetOps.addElement(active, 2)

  println(s"Before breakout:")
  println(s"  lab = ${lab.mkString(", ")}")
  println(s"  ptn = ${ptn.mkString(", ")}")
  println(s"  active has 0: ${SetOps.isElement(active, 0)}")
  println(s"  active has 2: ${SetOps.isElement(active, 2)}")

  // Simulate Scala's breakout (from Nauty.scala lines 704-722)
  val tcStart = 0
  val tv = 2
  val level = 1

  // Scala version:
  SetOps.emptySet(active, m)  // <-- This CLEARS active!
  SetOps.addElement(active, tcStart)

  var i = tcStart
  var prev = tv
  while (lab(i) != tv) {
    val next = lab(i)
    lab(i) = prev
    prev = next
    i += 1
  }
  lab(i) = prev
  ptn(tcStart) = level

  println(s"\nAfter Scala breakout:")
  println(s"  lab = ${lab.mkString(", ")}")
  println(s"  ptn = ${ptn.mkString(", ")}")
  println(s"  active has 0: ${SetOps.isElement(active, 0)}")
  println(s"  active has 2: ${SetOps.isElement(active, 2)}")  // Should be TRUE if C behavior

  println()
  println("PROBLEM: Scala clears active set, losing element 2!")
  println("C nauty would preserve active[2] = true")
  println()

  // Test the actual labeling swap
  println("Testing lab permutation:")
  println("-" * 40)
  val lab2 = Array(0, 1, 2, 3, 4)
  val tc2 = 0
  val tv2 = 3

  println(s"Before: lab = ${lab2.mkString(", ")}")
  println(s"Move vertex $tv2 to position $tc2")

  // Simulate Scala breakout swap
  i = tc2
  prev = tv2
  while (lab2(i) != tv2) {
    val next = lab2(i)
    lab2(i) = prev
    prev = next
    i += 1
  }
  lab2(i) = prev

  println(s"After:  lab = ${lab2.mkString(", ")}")
  println(s"Expected: lab = [3, 0, 1, 2, 4]")

  val expected = Array(3, 0, 1, 2, 4)
  if (lab2.sameElements(expected)) {
    println("Lab permutation: CORRECT")
  } else {
    println("Lab permutation: INCORRECT")
  }
  println()

  // Now let's look at what C nauty's breakout does
  println("C nauty breakout code (from nauty.c):")
  println("-" * 40)
  println("""
  void breakout(int *lab, int *ptn, int level, int tc, int tv, set *active, int m)
  {
      int i, prev, next;

      ADDELEMENT(active, tc);  // Add tc to active, don't clear!

      i = tc;
      prev = tv;

      while (lab[i] != tv) {
          next = lab[i];
          lab[i] = prev;
          prev = next;
          ++i;
      }
      lab[i] = prev;
      ptn[tc] = level;
  }
  """)
}

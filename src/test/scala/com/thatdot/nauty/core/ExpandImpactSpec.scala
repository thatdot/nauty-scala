package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.group.{Permutation, SchreierSims, BSGS}

/**
 * Test to demonstrate the impact of calling expand multiple times,
 * like the C code does in grouporder().
 */
class ExpandImpactSpec extends AnyFlatSpec with Matchers {

  // Manually compute BSGS with configurable expansion
  def computeBSGSWithExpansions(generators: Seq[Permutation], n: Int, numExpansions: Int): BigInt = {
    if (generators.isEmpty || n == 0) return BigInt(1)

    val bsgs = new TestBSGSBuilder(n)

    for (gen <- generators if !gen.isIdentity) {
      bsgs.addGenerator(gen)
    }

    // Call expand the specified number of times (like C does twice)
    for (_ <- 0 until numExpansions) {
      bsgs.expand()
    }

    bsgs.order
  }

  "Calling expand multiple times" should "improve accuracy for S7 adjacent transpositions" in {
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

    println("Testing S7 with different numbers of expand() calls:")
    println("(Each row shows results from 20 runs)")

    for (numExpansions <- 1 to 4) {
      var correct = 0
      var wrongValues = scala.collection.mutable.Set[BigInt]()

      for (_ <- 1 to 20) {
        val order = computeBSGSWithExpansions(gens, n, numExpansions)
        if (order == expected) correct += 1
        else wrongValues += order
      }

      val pct = correct * 100 / 20
      println(f"  expand() x $numExpansions: $correct%2d/20 correct ($pct%3d%%) ${if (wrongValues.nonEmpty) s"wrong: ${wrongValues.mkString(", ")}" else ""}")
    }
  }

  it should "improve accuracy for S6 adjacent transpositions" in {
    val gens = List(
      Permutation.fromArray(Array(0, 1, 2, 3, 4, 6, 5)),
      Permutation.fromArray(Array(0, 1, 2, 3, 5, 4, 6)),
      Permutation.fromArray(Array(0, 1, 2, 4, 3, 5, 6)),
      Permutation.fromArray(Array(0, 1, 3, 2, 4, 5, 6)),
      Permutation.fromArray(Array(0, 2, 1, 3, 4, 5, 6))
    )
    val n = 7
    val expected = BigInt(720)

    println("\nTesting S6 with different numbers of expand() calls:")

    for (numExpansions <- 1 to 4) {
      var correct = 0
      var wrongValues = scala.collection.mutable.Set[BigInt]()

      for (_ <- 1 to 20) {
        val order = computeBSGSWithExpansions(gens, n, numExpansions)
        if (order == expected) correct += 1
        else wrongValues += order
      }

      val pct = correct * 100 / 20
      println(f"  expand() x $numExpansions: $correct%2d/20 correct ($pct%3d%%) ${if (wrongValues.nonEmpty) s"wrong: ${wrongValues.mkString(", ")}" else ""}")
    }
  }
}

// Copy of BSGSBuilder that exposes expand() for testing
import scala.collection.mutable
import scala.util.Random

class TestBSGSBuilder(val n: Int) {
  private val levels = mutable.ListBuffer[TestLevelBuilder]()
  private val random = new Random()
  private val maxFails = 10  // Match C's SCHREIERFAILS

  def addGenerator(gen: Permutation): Boolean = {
    if (gen.isIdentity) return false
    siftAndAdd(gen)
  }

  private def siftAndAdd(perm: Permutation): Boolean = {
    var current = perm
    var levelIdx = 0
    var changed = false

    while (!current.isIdentity) {
      if (levelIdx >= levels.length) {
        val beta = (0 until n).find(i => current(i) != i).get
        levels += new TestLevelBuilder(beta, n)
        changed = true
      }

      val level = levels(levelIdx)
      val beta = level.basePoint
      val image = current(beta)

      level.transversal.get(image) match {
        case Some(rep) =>
          current = rep.inverse.compose(current)
          levelIdx += 1
        case None =>
          level.addGenerator(current)
          changed = true
          level.transversal.get(image) match {
            case Some(rep) =>
              current = rep.inverse.compose(current)
              levelIdx += 1
            case None =>
              throw new IllegalStateException("Failed to extend orbit")
          }
      }
    }
    changed
  }

  def expand(): Boolean = {
    var fails = 0
    var changed = false

    while (fails < maxFails) {
      if (expandOnce()) {
        changed = true
        fails = 0
      } else {
        fails += 1
      }
    }
    changed
  }

  private def expandOnce(): Boolean = {
    val allGens = levels.flatMap(_.generators.toSeq).toIndexedSeq
    if (allGens.isEmpty) return false

    var changed = false

    // Random product
    val wordLen = 1 + random.nextInt(4)
    var product = Permutation.identity(n)
    for (_ <- 0 until wordLen) {
      product = product.compose(allGens(random.nextInt(allGens.length)))
    }
    if (siftAndAdd(product)) changed = true

    // Schreier generators
    val levelsCopy = levels.toList
    for (level <- levelsCopy) {
      val orbitList = level.transversal.keys.toList
      val gensList = level.generators.toList

      for (gen <- gensList; pt <- orbitList) {
        val image = gen(pt)
        (level.transversal.get(pt), level.transversal.get(image)) match {
          case (Some(u), Some(v)) =>
            val schreierGen = v.inverse.compose(gen).compose(u)
            if (!schreierGen.isIdentity && siftAndAdd(schreierGen)) {
              changed = true
            }
          case _ =>
        }
      }
    }
    changed
  }

  def order: BigInt = {
    if (levels.isEmpty) BigInt(1)
    else levels.map(level => BigInt(level.transversal.size)).product
  }

  private class TestLevelBuilder(val basePoint: Int, n: Int) {
    val transversal = mutable.Map[Int, Permutation](basePoint -> Permutation.identity(n))
    val generators = mutable.Set[Permutation]()

    def addGenerator(gen: Permutation): Unit = {
      generators += gen
      val queue = mutable.Queue[Int]()
      val currentOrbit = transversal.keySet.toSet
      for (pt <- currentOrbit) queue.enqueue(pt)

      while (queue.nonEmpty) {
        val pt = queue.dequeue()
        val rep = transversal(pt)
        for (g <- generators) {
          val image = g(pt)
          if (!transversal.contains(image)) {
            transversal(image) = g.compose(rep)
            queue.enqueue(image)
          }
        }
      }
    }
  }
}

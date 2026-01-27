package com.thatdot.nauty.core

import com.thatdot.nauty.graph.DenseGraph
import com.thatdot.nauty.group.{Permutation, Orbits}

/**
 * Test to verify if redundant automorphisms are being recorded.
 */
object RedundancyTest extends App {

  println("=== Redundancy Test ===\n")

  // For K3, C nauty finds generators (1 2) and (0 1)
  // Scala finds (1 2), (0 1), and (0 1 2)

  // Check: (0 1 2) = (0 1) * (1 2)?
  val g01 = Permutation.fromArray(Array(1, 0, 2))  // (0 1)
  val g12 = Permutation.fromArray(Array(0, 2, 1))  // (1 2)
  val g012 = Permutation.fromArray(Array(1, 2, 0)) // (0 1 2)

  println("Checking K3 generators:")
  println(s"  g01 = (0 1) = ${g01.toArrayString}")
  println(s"  g12 = (1 2) = ${g12.toArrayString}")
  println(s"  g012 = (0 1 2) = ${g012.toArrayString}")
  println()

  // g01 * g12: first apply g12, then g01
  // g12: 0->0, 1->2, 2->1
  // g01: 0->1, 1->0, 2->2
  // Composition: 0 -> g01(g12(0)) = g01(0) = 1
  //              1 -> g01(g12(1)) = g01(2) = 2
  //              2 -> g01(g12(2)) = g01(1) = 0
  // Result: [1, 2, 0] = (0 1 2)

  val product = g01.compose(g12)
  println(s"  g01 * g12 = ${product.toArrayString} = ${product.toCycleString}")
  println(s"  This equals g012? ${product == g012}")
  println()

  if (product == g012) {
    println("CONFIRMED: (0 1 2) is a PRODUCT of (0 1) and (1 2)")
    println("Scala should NOT be recording it as a separate generator!")
    println()
  }

  // Now trace what Scala's isAutomorphism() does
  println("Tracing Scala's automorphism detection:")
  println("-" * 40)

  val k3 = DenseGraph.complete(3)

  // The issue is that isAutomorphism() checks if the permutation preserves edges
  // This is CORRECT - (0 1 2) does preserve K3's edges
  // But the problem is it shouldn't be recorded as a NEW generator

  println("All three permutations ARE automorphisms of K3:")
  for (p <- Seq(g01, g12, g012)) {
    var valid = true
    for (i <- 0 until 3; j <- k3.neighbors(i)) {
      if (!k3.hasEdge(p(i), p(j))) valid = false
    }
    println(s"  ${p.toCycleString} is automorphism: $valid")
  }

  println()
  println("The problem is NOT in isAutomorphism()")
  println("The problem is that Scala records ALL automorphisms found at leaves")
  println("instead of only recording those that extend the generating set")
  println()

  // Show C nauty's approach
  println("C nauty's approach:")
  println("-" * 40)
  println("""
In C nauty, automorphisms are only recorded when:
1. The current leaf matches the first leaf's code up to a certain level
2. The permutation maps the first leaf to the current leaf
3. The orbit-based pruning indicates this is a new coset representative

The key variable 'gca_first' (greatest common ancestor with first)
tracks where paths diverge. Only automorphisms that correspond to
genuine new coset representatives are recorded.

Scala's code doesn't properly track this - it records ALL valid
automorphisms, including those that are products of earlier ones.
""")

  // Demonstrate with orbit tracking
  println("\nOrbit-based group size calculation:")
  println("-" * 40)

  val orbits = Orbits.identity(3)
  println(s"Initial orbits: ${orbits.mkString(", ")} (3 orbits)")

  // First automorphism: (1 2)
  var numOrbits = Orbits.orbjoin(orbits, Array(0, 2, 1), 3)
  println(s"After (1 2): orbits = ${orbits.mkString(", ")} ($numOrbits orbits)")
  println(s"  C nauty index: 2 (target cell had 2 elements)")

  // Second automorphism: (0 1)
  numOrbits = Orbits.orbjoin(orbits, Array(1, 0, 2), 3)
  println(s"After (0 1): orbits = ${orbits.mkString(", ")} ($numOrbits orbits)")
  println(s"  C nauty index: 3 (target cell had 3 elements)")

  // Third automorphism: (0 1 2) - SHOULD NOT BE RECORDED
  numOrbits = Orbits.orbjoin(orbits, Array(1, 2, 0), 3)
  println(s"After (0 1 2): orbits = ${orbits.mkString(", ")} ($numOrbits orbits)")
  println(s"  No orbit change - C nauty would not record this!")

  println("\nC nauty group size: 2 * 3 = 6")
  println("Scala group size: uses wrong heuristic = 8")
}

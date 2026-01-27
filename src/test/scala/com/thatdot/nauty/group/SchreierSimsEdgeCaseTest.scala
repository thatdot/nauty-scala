package com.thatdot.nauty.group

/**
 * Edge case tests specifically targeting potential algorithmic issues
 * in the Schreier-Sims implementation.
 */
object SchreierSimsEdgeCaseTest extends App {
  
  var failures = 0
  var passes = 0
  
  def test(name: String)(body: => Boolean): Unit = {
    print(s"$name... ")
    try {
      if (body) { println("PASS"); passes += 1 }
      else { println("FAIL"); failures += 1 }
    } catch { case e: Exception => println(s"ERROR: ${e.getMessage}"); failures += 1 }
  }
  
  println("=" * 70)
  println("SchreierSims Edge Case Tests")
  println("=" * 70)
  
  // Test 1: Generator added at deeper level affects orbit at earlier level
  // This tests if the orbit computation properly includes all generators
  println("\n--- Cross-level Generator Tests ---")
  
  // Group where adding generators in certain order might miss orbit elements
  test("Order independent orbit computation") {
    // Create a group where generator order matters for naive implementations
    val g1 = Permutation.fromCycles(6, List(0, 1))        // swap 0,1
    val g2 = Permutation.fromCycles(6, List(2, 3))        // swap 2,3
    val g3 = Permutation.fromCycles(6, List(0, 2), List(1, 3)) // swap pairs
    
    // Both orderings should give same result
    val bsgs1 = SchreierSims.computeBSGS(List(g1, g2, g3), 6)
    val bsgs2 = SchreierSims.computeBSGS(List(g3, g2, g1), 6)
    val bsgs3 = SchreierSims.computeBSGS(List(g2, g3, g1), 6)
    
    bsgs1.order == BigInt(8) && bsgs2.order == BigInt(8) && bsgs3.order == BigInt(8)
  }
  
  // Test 2: Complex interleaved generators
  test("Complex generator interleaving") {
    // Generators that interact in complex ways
    val g1 = Permutation.fromCycles(5, List(0, 1, 2))     // 3-cycle
    val g2 = Permutation.fromCycles(5, List(2, 3, 4))     // another 3-cycle
    val g3 = Permutation.fromCycles(5, List(1, 3))        // transposition
    
    val bsgs = SchreierSims.computeBSGS(List(g1, g2, g3), 5)
    
    // This should generate A5 (alternating group) since all generators are even
    // Wait, (1 3) is odd, so this generates S5
    bsgs.order == BigInt(120)  // S5
  }
  
  // Test 3: Redundant generators that create same orbit elements
  test("Highly redundant generators") {
    // All transpositions generate S4, but adding them all should still work
    val gens = for (i <- 0 until 4; j <- i+1 until 4) 
      yield Permutation.transposition(4, i, j)
    
    val bsgs = SchreierSims.computeBSGS(gens.toList, 4)
    bsgs.order == BigInt(24)
  }
  
  // Test 4: Single generator of large order
  test("Large order cyclic generator") {
    // Generator of order 12
    val gen = Permutation.cyclic(12)
    val bsgs = SchreierSims.computeBSGS(List(gen), 12)
    
    // Should have exactly 12 elements
    val elements = bsgs.elements().toSet
    elements.size == 12 && bsgs.order == BigInt(12)
  }
  
  // Test 5: Generator that becomes identity after sifting
  test("Generator that reduces to identity") {
    // Add (0 1) twice - second should be redundant
    val gen = Permutation.transposition(4, 0, 1)
    val bsgs = SchreierSims.computeBSGS(List(gen, gen), 4)
    bsgs.order == BigInt(2)
  }
  
  // Test 6: Verify enumeration produces exactly the group elements
  println("\n--- Enumeration Exactness Tests ---")
  test("A4 enumeration exact") {
    val gens = List(
      Permutation.fromCycles(4, List(0, 1, 2)),
      Permutation.fromCycles(4, List(0, 2, 3))
    )
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    val elements = bsgs.elements().toSet
    
    // A4 has exactly 12 elements
    // All should be even permutations (equal number of even cycles)
    val allEven = elements.forall { p =>
      val sign = p.cycles.map(_.length - 1).sum % 2
      sign == 0
    }
    
    elements.size == 12 && allEven
  }
  
  // Test 7: Membership of complex products
  println("\n--- Complex Product Membership ---")
  test("Long product of generators is member") {
    val gens = List(Permutation.transposition(5, 0, 1), Permutation.cyclic(5))
    val bsgs = SchreierSims.computeBSGS(gens, 5)
    
    // Compute g0^3 * g1^7 * g0^2 * g1^4
    var product = Permutation.identity(5)
    for (_ <- 1 to 3) product = product.compose(gens(0))
    for (_ <- 1 to 7) product = product.compose(gens(1))
    for (_ <- 1 to 2) product = product.compose(gens(0))
    for (_ <- 1 to 4) product = product.compose(gens(1))
    
    bsgs.contains(product)
  }
  
  // Test 8: Inverses of complex products
  test("Inverse of long product is member") {
    val gens = List(Permutation.transposition(5, 0, 1), Permutation.cyclic(5))
    val bsgs = SchreierSims.computeBSGS(gens, 5)
    
    var product = Permutation.identity(5)
    for (_ <- 1 to 5) product = product.compose(gens(0).compose(gens(1)))
    
    bsgs.contains(product.inverse)
  }
  
  // Test 9: Verify that non-members are consistently rejected
  println("\n--- Non-member Rejection ---")
  test("Odd permutation not in A4") {
    val a4gens = List(
      Permutation.fromCycles(4, List(0, 1, 2)),
      Permutation.fromCycles(4, List(0, 2, 3))
    )
    val bsgs = SchreierSims.computeBSGS(a4gens, 4)
    
    // (0 1) is an odd permutation, should not be in A4
    val oddPerm = Permutation.transposition(4, 0, 1)
    !bsgs.contains(oddPerm)
  }
  
  // Test 10: Verify BSGS structure invariants
  println("\n--- BSGS Structure Invariants ---")
  test("Orbit sizes multiply to group order") {
    val gens = List(Permutation.transposition(5, 0, 1), Permutation.cyclic(5))
    val bsgs = SchreierSims.computeBSGS(gens, 5)
    
    val productOfOrbitSizes = bsgs.levels.map(l => BigInt(l.orbitSize)).product
    productOfOrbitSizes == bsgs.order
  }
  
  test("Each orbit contains base point") {
    val gens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    
    bsgs.levels.forall(l => l.orbit.contains(l.basePoint))
  }
  
  test("Identity maps base point to itself") {
    val gens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    
    bsgs.levels.forall { l =>
      l.transversal.get(l.basePoint) match {
        case Some(rep) => rep.isIdentity
        case None => false
      }
    }
  }
  
  println()
  println("=" * 70)
  println(s"Results: $passes passed, $failures failed")
  println("=" * 70)
  if (failures > 0) System.exit(1)
}

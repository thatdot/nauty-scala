package com.thatdot.nauty.group

/**
 * Stress tests for SchreierSims to find potential bugs.
 */
object SchreierSimsStressTest extends App {
  
  var failures = 0
  var passes = 0
  
  def test(name: String)(body: => Boolean): Unit = {
    print(s"Testing $name... ")
    try {
      if (body) {
        println("PASS")
        passes += 1
      } else {
        println("FAIL")
        failures += 1
      }
    } catch {
      case e: Exception =>
        println(s"ERROR: ${e.getMessage}")
        e.printStackTrace()
        failures += 1
    }
  }
  
  def testOrder(name: String, gens: Seq[Permutation], n: Int, expected: BigInt): Unit = {
    test(s"$name order = $expected") {
      val order = SchreierSims.groupOrder(gens, n)
      if (order != expected) {
        println(s"Expected $expected, got $order")
        false
      } else true
    }
  }
  
  def testEnumeration(name: String, gens: Seq[Permutation], n: Int, expectedOrder: BigInt): Unit = {
    test(s"$name enumeration") {
      if (expectedOrder > 10000) {
        println("(skipped - too large)")
        true
      } else {
        val bsgs = SchreierSims.computeBSGS(gens, n)
        val elements = bsgs.elements().toSet
        val orderMatch = elements.size == expectedOrder.toInt
        if (!orderMatch) {
          println(s"Order mismatch: enumerated ${elements.size}, expected $expectedOrder")
        }
        orderMatch
      }
    }
  }
  
  println("=" * 60)
  println("SchreierSims Stress Test")
  println("=" * 60)
  println()
  
  // Test 1: Symmetric groups S_n
  println("--- Symmetric Groups ---")
  for (n <- 2 to 7) {
    val gens = List(
      Permutation.transposition(n, 0, 1),
      Permutation.cyclic(n)
    )
    val factorial = (1 to n).map(BigInt(_)).product
    testOrder(s"S$n", gens, n, factorial)
    testEnumeration(s"S$n", gens, n, factorial)
  }
  
  // Test 2: Alternating groups A_n
  println("\n--- Alternating Groups ---")
  for (n <- 3 to 6) {
    val gens = (0 until n - 2).map { i =>
      Permutation.fromCycles(n, List(i, i + 1, i + 2))
    }.toList
    val expected = (1 to n).map(BigInt(_)).product / 2
    testOrder(s"A$n", gens, n, expected)
    testEnumeration(s"A$n", gens, n, expected)
  }
  
  // Test 3: Dihedral groups D_n
  println("\n--- Dihedral Groups ---")
  for (n <- 3 to 8) {
    val rotation = Permutation.cyclic(n)
    val reflection = Permutation.fromArray((0 until n).map(i => if (i == 0) 0 else n - i).toArray)
    val gens = List(rotation, reflection)
    testOrder(s"D$n", gens, n, BigInt(2 * n))
    testEnumeration(s"D$n", gens, n, BigInt(2 * n))
  }
  
  // Test 4: Cyclic groups Z_n
  println("\n--- Cyclic Groups ---")
  for (n <- 2 to 10) {
    val gens = List(Permutation.cyclic(n))
    testOrder(s"Z$n", gens, n, BigInt(n))
  }
  
  // Test 5: Direct product Z_2^3
  println("\n--- Product Groups ---")
  val g1 = Permutation.fromCycles(6, List(0, 1))
  val g2 = Permutation.fromCycles(6, List(2, 3))
  val g3 = Permutation.fromCycles(6, List(4, 5))
  testOrder("Z2^3", List(g1, g2, g3), 6, BigInt(8))
  testEnumeration("Z2^3", List(g1, g2, g3), 6, BigInt(8))
  
  // Test 6: Wreath product S_2 wr S_2
  println("\n--- Wreath Products ---")
  val w1 = Permutation.fromCycles(4, List(0, 1))
  val w2 = Permutation.fromCycles(4, List(2, 3))
  val w3 = Permutation.fromCycles(4, List(0, 2), List(1, 3))
  testOrder("S2 wr S2", List(w1, w2, w3), 4, BigInt(8))
  testEnumeration("S2 wr S2", List(w1, w2, w3), 4, BigInt(8))
  
  // Test 7: Redundant generators
  println("\n--- Redundant Generators ---")
  val allTranspositions = for {
    i <- 0 until 4
    j <- i + 1 until 4
  } yield Permutation.transposition(4, i, j)
  testOrder("S4 with all transpositions", allTranspositions.toList, 4, BigInt(24))
  
  // Test 8: Non-member detection
  println("\n--- Non-member Detection ---")
  val subgens = List(Permutation.transposition(4, 0, 1))
  val subbsgs = SchreierSims.computeBSGS(subgens, 4)
  
  test("(0 2) not in <(0 1)>") {
    !subbsgs.contains(Permutation.transposition(4, 0, 2))
  }
  test("(0 1 2) not in <(0 1)>") {
    !subbsgs.contains(Permutation.fromCycles(4, List(0, 1, 2)))
  }
  
  // Test 9: Edge cases
  println("\n--- Edge Cases ---")
  test("Empty generator list") {
    SchreierSims.groupOrder(Nil, 5) == BigInt(1)
  }
  test("n=1 (trivial)") {
    SchreierSims.groupOrder(Nil, 1) == BigInt(1)
  }
  test("n=0") {
    SchreierSims.groupOrder(Nil, 0) == BigInt(1)
  }
  
  // Test 10: Sifting correctness
  println("\n--- Sifting Correctness ---")
  val siftgens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
  val siftbsgs = SchreierSims.computeBSGS(siftgens, 4)
  
  test("Sift of identity gives identity residue") {
    val (residue, isId) = siftbsgs.sift(Permutation.identity(4))
    isId && residue.isIdentity
  }
  test("Sift of generator gives identity residue") {
    val (residue, isId) = siftbsgs.sift(siftgens(0))
    isId && residue.isIdentity
  }
  test("Sift of non-member gives non-identity") {
    val subgens2 = List(Permutation.transposition(4, 0, 1))
    val subbsgs2 = SchreierSims.computeBSGS(subgens2, 4)
    val nonMember = Permutation.transposition(4, 0, 2)
    val (_, isId) = subbsgs2.sift(nonMember)
    !isId
  }
  
  // Test 11: Order/Enumeration consistency
  println("\n--- Order/Enumeration Consistency ---")
  val testCases = List(
    ("S3", List(Permutation.transposition(3, 0, 1), Permutation.cyclic(3)), 3),
    ("A4", List(Permutation.fromCycles(4, List(0, 1, 2)), Permutation.fromCycles(4, List(0, 2, 3))), 4),
    ("D5", List(Permutation.cyclic(5), Permutation.fromArray(Array(0, 4, 3, 2, 1))), 5)
  )
  for ((name, gens, n) <- testCases) {
    test(s"$name: order == enumeration count") {
      val bsgs = SchreierSims.computeBSGS(gens, n)
      val order = bsgs.order
      val enumCount = bsgs.elements().toSet.size
      order == BigInt(enumCount)
    }
  }
  
  // Test 12: Verify all enumerated elements are valid permutations and in the group
  println("\n--- Enumeration Validity ---")
  test("All S4 elements are valid and distinct") {
    val gens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    val elements = bsgs.elements().toList
    val distinct = elements.toSet.size == elements.size
    val allValid = elements.forall { p =>
      // Check it's a valid permutation
      val arr = p.toArray
      arr.sorted.sameElements(Array(0, 1, 2, 3))
    }
    distinct && allValid && elements.size == 24
  }
  
  println()
  println("=" * 60)
  println(s"Results: $passes passed, $failures failed")
  println("=" * 60)
  
  if (failures > 0) System.exit(1)
}

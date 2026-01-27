package com.thatdot.nauty.group

object SchreierSimsComparisonTest extends App {
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
  println("SchreierSims Comparison Tests")
  println("=" * 70)
  
  // Test closure
  println("\n--- Closure Tests ---")
  test("S3 closure") {
    val gens = List(Permutation.transposition(3, 0, 1), Permutation.cyclic(3))
    val bsgs = SchreierSims.computeBSGS(gens, 3)
    val elements = bsgs.elements().toList
    elements.forall(g => elements.forall(h => bsgs.contains(g.compose(h))))
  }
  
  // Test inverses
  println("\n--- Inverse Tests ---")
  test("S4 inverses") {
    val gens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    bsgs.elements().forall(g => bsgs.contains(g.inverse))
  }
  
  // Stabilizer chain validity
  println("\n--- Stabilizer Chain Tests ---")
  test("S4 stabilizer chain valid") {
    val gens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    var ok = true
    for ((level, idx) <- bsgs.levels.zipWithIndex) {
      val prevBases = bsgs.levels.take(idx).map(_.basePoint)
      for (gen <- level.generators; bp <- prevBases) {
        if (gen(bp) != bp) ok = false
      }
    }
    ok
  }
  
  // Transversal correctness
  println("\n--- Transversal Tests ---")
  test("Transversal maps base point correctly") {
    val gens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    var ok = true
    for (level <- bsgs.levels) {
      val bp = level.basePoint
      for ((target, rep) <- level.transversal) {
        if (rep(bp) != target) ok = false
      }
    }
    ok
  }
  
  // Randomization consistency
  println("\n--- Consistency Tests ---")
  test("Multiple runs same order") {
    val gens = List(Permutation.transposition(5, 0, 1), Permutation.cyclic(5))
    val orders = (1 to 10).map(_ => SchreierSims.computeBSGS(gens, 5).order)
    orders.distinct.size == 1 && orders.head == BigInt(120)
  }
  
  // Sifting tests
  println("\n--- Sifting Tests ---")
  test("Sift gives identity for members") {
    val gens = List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4))
    val bsgs = SchreierSims.computeBSGS(gens, 4)
    bsgs.elements().take(24).forall { elem =>
      val (residue, isId) = bsgs.sift(elem)
      isId && residue.isIdentity
    }
  }
  
  test("Sift gives non-identity for non-members") {
    val gens = List(Permutation.transposition(5, 0, 1))
    val bsgs = SchreierSims.computeBSGS(gens, 5)
    val nonMember = Permutation.transposition(5, 0, 2)
    val (_, isId) = bsgs.sift(nonMember)
    !isId
  }
  
  // A4 subgroup of S4
  println("\n--- Subgroup Tests ---")
  test("A4 subset of S4") {
    val s4bsgs = SchreierSims.computeBSGS(List(Permutation.transposition(4, 0, 1), Permutation.cyclic(4)), 4)
    val a4gens = List(Permutation.fromCycles(4, List(0, 1, 2)), Permutation.fromCycles(4, List(0, 2, 3)))
    val a4bsgs = SchreierSims.computeBSGS(a4gens, 4)
    a4bsgs.elements().forall(s4bsgs.contains)
  }
  
  println()
  println("=" * 70)
  println(s"Results: $passes passed, $failures failed")
  println("=" * 70)
  if (failures > 0) System.exit(1)
}

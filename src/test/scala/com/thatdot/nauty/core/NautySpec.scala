package com.thatdot.nauty.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.DenseGraph
import com.thatdot.nauty.util.NautyOptions
import com.thatdot.nauty.group.Permutation

class NautySpec extends AnyFlatSpec with Matchers {

  // Helper to compute factorial
  private def factorial(n: Int): Int = if (n <= 1) 1 else n * factorial(n - 1)

  // Helper to verify a permutation is an automorphism of graph g
  private def isAutomorphism(g: DenseGraph, perm: Permutation): Boolean = {
    val n = g.n
    for (i <- 0 until n; j <- 0 until n) {
      val hasOriginal = g.hasEdge(i, j)
      val hasPermuted = g.hasEdge(perm(i), perm(j))
      if (hasOriginal != hasPermuted) return false
    }
    true
  }

  // Helper to verify exact group size by enumeration (for small graphs)
  private def verifyGroupSizeByEnumeration(g: DenseGraph, expectedSize: Int): Unit = {
    val result = Nauty.densenauty(g)
    val group = Nauty.generateGroup(result.generators, maxSize = expectedSize + 100)

    // All generated elements should be automorphisms
    for (perm <- group) {
      withClue(s"Permutation $perm should be an automorphism: ") {
        isAutomorphism(g, perm) shouldBe true
      }
    }

    // Group size should match
    // Note: generateGroup returns empty set for empty generators, but the trivial group
    // (identity only) has size 1, so we account for this
    val actualSize = if (group.isEmpty && result.generators.isEmpty) 1 else group.size
    withClue(s"Group size for graph with ${g.n} vertices, ${g.numEdges} edges: ") {
      actualSize shouldBe expectedSize
    }
  }

  //
  // Basic functionality tests
  //

  "Nauty.densenauty" should "handle empty graph (0 vertices)" in {
    val g = DenseGraph.empty(0)
    val result = Nauty.densenauty(g)

    result.numOrbits shouldBe 0
    result.groupSize shouldBe BigDecimal(1)
    result.generators shouldBe empty
  }

  it should "handle single vertex" in {
    val g = DenseGraph.empty(1)
    val result = Nauty.densenauty(g)

    result.numOrbits shouldBe 1
    result.groupSize shouldBe BigDecimal(1)
    result.orbits shouldBe Array(0)
    result.generators shouldBe empty
  }

  it should "handle two disconnected vertices" in {
    val g = DenseGraph.empty(2)
    val result = Nauty.densenauty(g)

    // Two isolated vertices are equivalent, group is S2 = Z2
    result.numOrbits shouldBe 1
    verifyGroupSizeByEnumeration(g, 2)
  }

  it should "handle single edge (K2)" in {
    val g = DenseGraph.fromEdges(2, Seq((0, 1)))
    val result = Nauty.densenauty(g)

    // K2 has automorphism group Z2 (swap endpoints)
    result.numOrbits shouldBe 1
    verifyGroupSizeByEnumeration(g, 2)
  }

  //
  // Complete graphs K_n: |Aut(K_n)| = n!
  //

  "Complete graph K3" should "have automorphism group S3 of order 6" in {
    val k3 = DenseGraph.complete(3)
    val result = Nauty.densenauty(k3)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(k3, factorial(3))  // 6
  }

  "Complete graph K4" should "have automorphism group S4 of order 24" in {
    val k4 = DenseGraph.complete(4)
    val result = Nauty.densenauty(k4)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(k4, factorial(4))  // 24
  }

  "Complete graph K5" should "have automorphism group S5 of order 120" in {
    val k5 = DenseGraph.complete(5)
    val result = Nauty.densenauty(k5)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(k5, factorial(5))  // 120
  }

  //
  // Empty graphs (no edges) E_n: |Aut(E_n)| = n!
  //

  "Empty graph on 4 vertices" should "have automorphism group S4 of order 24" in {
    val g = DenseGraph.empty(4)
    val result = Nauty.densenauty(g)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(g, factorial(4))  // 24
  }

  //
  // Cycle graphs C_n: |Aut(C_n)| = 2n (dihedral group D_n)
  //

  "Cycle graph C3 (triangle)" should "have automorphism group D3 of order 6" in {
    val c3 = DenseGraph.cycle(3)
    val result = Nauty.densenauty(c3)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(c3, 2 * 3)  // 6
  }

  "Cycle graph C4 (square)" should "have automorphism group D4 of order 8" in {
    val c4 = DenseGraph.cycle(4)
    val result = Nauty.densenauty(c4)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(c4, 2 * 4)  // 8
  }

  "Cycle graph C5 (pentagon)" should "have automorphism group D5 of order 10" in {
    val c5 = DenseGraph.cycle(5)
    val result = Nauty.densenauty(c5)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(c5, 2 * 5)  // 10
  }

  "Cycle graph C6 (hexagon)" should "have automorphism group D6 of order 12" in {
    val c6 = DenseGraph.cycle(6)
    val result = Nauty.densenauty(c6)

    result.numOrbits shouldBe 1  // All vertices equivalent
    verifyGroupSizeByEnumeration(c6, 2 * 6)  // 12
  }

  //
  // Path graphs P_n: |Aut(P_n)| = 2 (reflection only, for n >= 2)
  //

  "Path graph P2" should "have automorphism group Z2 of order 2" in {
    val p2 = DenseGraph.path(2)
    val result = Nauty.densenauty(p2)

    result.numOrbits shouldBe 1  // Both endpoints equivalent
    verifyGroupSizeByEnumeration(p2, 2)
  }

  "Path graph P3" should "have automorphism group Z2 of order 2" in {
    val p3 = DenseGraph.path(3)
    val result = Nauty.densenauty(p3)

    // Endpoints 0,2 are equivalent; middle vertex 1 is alone
    result.numOrbits shouldBe 2
    verifyGroupSizeByEnumeration(p3, 2)
  }

  "Path graph P4" should "have automorphism group Z2 of order 2" in {
    val p4 = DenseGraph.path(4)
    val result = Nauty.densenauty(p4)

    // 0,3 equivalent; 1,2 equivalent
    result.numOrbits shouldBe 2
    verifyGroupSizeByEnumeration(p4, 2)
  }

  "Path graph P5" should "have automorphism group Z2 of order 2" in {
    val p5 = DenseGraph.path(5)
    val result = Nauty.densenauty(p5)

    // 0,4 equivalent; 1,3 equivalent; 2 alone
    result.numOrbits shouldBe 3
    verifyGroupSizeByEnumeration(p5, 2)
  }

  //
  // Star graphs S_n (center + n-1 leaves): |Aut(S_n)| = (n-1)!
  //

  "Star graph S4 (center + 3 leaves)" should "have automorphism group S3 of order 6" in {
    // Star: vertex 0 connected to 1, 2, 3
    val star = DenseGraph.fromEdges(4, Seq((0, 1), (0, 2), (0, 3)))
    val result = Nauty.densenauty(star)

    // Center is in its own orbit, leaves are equivalent
    result.numOrbits shouldBe 2
    verifyGroupSizeByEnumeration(star, factorial(3))  // 6
  }

  "Star graph S5 (center + 4 leaves)" should "have automorphism group S4 of order 24" in {
    val star = DenseGraph.fromEdges(5, Seq((0, 1), (0, 2), (0, 3), (0, 4)))
    val result = Nauty.densenauty(star)

    result.numOrbits shouldBe 2
    verifyGroupSizeByEnumeration(star, factorial(4))  // 24
  }

  //
  // Petersen graph: |Aut| = 120 = |S5|
  //

  "Petersen graph" should "have automorphism group of order 120" in {
    // Petersen graph: 10 vertices
    // Outer pentagon: 0-1-2-3-4-0
    // Inner pentagram: 5-7-9-6-8-5
    // Spokes: 0-5, 1-6, 2-7, 3-8, 4-9
    val petersen = DenseGraph.fromEdges(10, Seq(
      // Outer pentagon
      (0, 1), (1, 2), (2, 3), (3, 4), (4, 0),
      // Inner pentagram
      (5, 7), (7, 9), (9, 6), (6, 8), (8, 5),
      // Spokes
      (0, 5), (1, 6), (2, 7), (3, 8), (4, 9)
    ))
    val result = Nauty.densenauty(petersen)

    result.numOrbits shouldBe 1  // All vertices equivalent (vertex-transitive)
    petersen.numEdges shouldBe 15

    // Petersen graph has automorphism group S5 of order 120
    verifyGroupSizeByEnumeration(petersen, 120)
  }

  //
  // Asymmetric graphs: |Aut| = 1 (only identity)
  //

  "Asymmetric graph" should "have trivial automorphism group" in {
    // A graph with no non-trivial automorphisms
    // This is the smallest asymmetric tree on 7 vertices:
    //   0 - 1 - 2 - 3
    //           |
    //           4 - 5 - 6
    // Each vertex has a unique position in the tree structure
    val g = DenseGraph.fromEdges(7, Seq(
      (0, 1), (1, 2), (2, 3), (2, 4), (4, 5), (5, 6)
    ))
    val result = Nauty.densenauty(g)

    // All vertices are in separate orbits (no equivalences)
    result.numOrbits shouldBe 7
    result.generators shouldBe empty
    verifyGroupSizeByEnumeration(g, 1)
  }

  //
  // Generator validity tests
  //

  "Nauty generators" should "all be valid automorphisms" in {
    val graphs = Seq(
      DenseGraph.complete(5),
      DenseGraph.cycle(6),
      DenseGraph.path(7),
      DenseGraph.fromEdges(6, Seq((0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3), (0, 3)))
    )

    for (g <- graphs) {
      val result = Nauty.densenauty(g)
      for (gen <- result.generators) {
        withClue(s"Generator ${gen.toCycleString} for graph with ${g.n} vertices: ") {
          isAutomorphism(g, gen) shouldBe true
        }
      }
    }
  }

  //
  // Canonical form tests
  //

  "Nauty.canonicalForm" should "produce same result for isomorphic graphs" in {
    val g1 = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3)))
    val g2 = DenseGraph.fromEdges(4, Seq((3, 2), (2, 1), (1, 0)))

    val opts = NautyOptions.defaultGraph.withCanon
    val r1 = Nauty.densenauty(g1, opts)
    val r2 = Nauty.densenauty(g2, opts)

    r1.canonicalGraph shouldBe defined
    r2.canonicalGraph shouldBe defined
    r1.canonicalGraph.get shouldBe r2.canonicalGraph.get
  }

  it should "produce consistent canonical forms for all labelings of C4" in {
    val perms = Seq(
      Array(0, 1, 2, 3),
      Array(1, 2, 3, 0),
      Array(2, 3, 0, 1),
      Array(3, 0, 1, 2),
      Array(0, 3, 2, 1),  // reflection
      Array(1, 0, 3, 2),
      Array(2, 1, 0, 3),
      Array(3, 2, 1, 0)
    )

    val baseEdges = Seq((0, 1), (1, 2), (2, 3), (3, 0))
    val canonForms = perms.map { perm =>
      val edges = baseEdges.map { case (a, b) =>
        val i = perm.indexOf(a)
        val j = perm.indexOf(b)
        (i, j)
      }
      val g = DenseGraph.fromEdges(4, edges)
      Nauty.canonicalForm(g)
    }

    // All should be the same
    for (i <- 1 until canonForms.length) {
      canonForms(i) shouldBe canonForms(0)
    }
  }

  //
  // Isomorphism detection tests
  //

  "Nauty.isIsomorphic" should "detect isomorphic graphs" in {
    val g1 = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3), (3, 0)))
    val g2 = DenseGraph.fromEdges(4, Seq((0, 2), (2, 1), (1, 3), (3, 0)))

    Nauty.isIsomorphic(g1, g2) shouldBe true
  }

  it should "detect non-isomorphic graphs with same vertex/edge count" in {
    // Two non-isomorphic graphs on 6 vertices with 6 edges
    val g1 = DenseGraph.cycle(6)  // C6
    val g2 = DenseGraph.fromEdges(6, Seq((0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3)))  // Two triangles

    Nauty.isIsomorphic(g1, g2) shouldBe false
  }

  it should "detect non-isomorphic graphs (path vs star)" in {
    val path = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3)))
    val star = DenseGraph.fromEdges(4, Seq((0, 1), (0, 2), (0, 3)))

    Nauty.isIsomorphic(path, star) shouldBe false
  }

  it should "correctly compare complete graphs" in {
    val k4_1 = DenseGraph.complete(4)
    val k4_2 = DenseGraph.complete(4)
    val k5 = DenseGraph.complete(5)

    Nauty.isIsomorphic(k4_1, k4_2) shouldBe true
    Nauty.isIsomorphic(k4_1, k5) shouldBe false
  }

  //
  // Partition tests
  //

  "Partition" should "be constructed correctly" in {
    val p = Partition.trivial(5)
    p.n shouldBe 5
    p.numCells() shouldBe 1
    p.isDiscrete shouldBe false

    val p2 = Partition.discrete(5)
    p2.numCells() shouldBe 5
    p2.isDiscrete shouldBe true
  }

  it should "support cell operations" in {
    val p = Partition.fromCells(5, Seq(Seq(0, 1), Seq(2), Seq(3, 4)))
    p.numCells() shouldBe 3
    p.cells().map(_.toSet) shouldBe IndexedSeq(Set(0, 1), Set(2), Set(3, 4))
  }

  //
  // Partition-restricted automorphism tests
  //

  "Nauty with initial partition" should "restrict automorphisms to partition-preserving ones" in {
    val k4 = DenseGraph.complete(4)

    // Without partition: full S4 group of order 24
    val r1 = Nauty.densenauty(k4)
    r1.numOrbits shouldBe 1

    // With partition [0,1] [2,3]: only swaps within cells, group is Z2 x Z2 of order 4
    val opts = NautyOptions.defaultGraph.withPartition(Seq(Seq(0, 1), Seq(2, 3)))
    val r2 = Nauty.densenauty(k4, opts)
    r2.numOrbits shouldBe 2  // 0,1 in one orbit; 2,3 in another

    // Verify the restricted group by enumeration
    val group = Nauty.generateGroup(r2.generators, maxSize = 100)

    // All group elements should preserve the partition structure
    for (perm <- group) {
      val cell0Image = Set(perm(0), perm(1))
      // Each cell should map to a valid cell (since K4 is complete, cells can swap)
      (cell0Image == Set(0, 1) || cell0Image == Set(2, 3)) shouldBe true
    }

    // For K4 with partition [0,1][2,3], we can swap within cells AND swap cells
    // So group size should be 2 * 2 * 2 = 8 (swap 0-1, swap 2-3, swap cells)
    // Wait, swapping cells means (0,2)(1,3), so it's (Z2 x Z2) x Z2 = order 8
    // Actually for K4, swapping cells IS an automorphism, so we should get 8
  }

  it should "give trivial group for discrete partition" in {
    val k4 = DenseGraph.complete(4)
    val opts = NautyOptions.defaultGraph.withPartition(Seq(Seq(0), Seq(1), Seq(2), Seq(3)))
    val result = Nauty.densenauty(k4, opts)

    // With discrete partition, only identity preserves it
    result.numOrbits shouldBe 4
    result.generators shouldBe empty
  }

  //
  // Nauty.generateGroup tests
  //

  "Nauty.generateGroup" should "generate Z3 from single 3-cycle" in {
    val gen = List(Permutation.fromCycles(3, List(0, 1, 2)))
    val group = Nauty.generateGroup(gen)

    group.size shouldBe 3
    group should contain(Permutation.identity(3))
    group should contain(Permutation.fromCycles(3, List(0, 1, 2)))
    group should contain(Permutation.fromCycles(3, List(0, 2, 1)))
  }

  it should "generate S3 from transposition and 3-cycle" in {
    val gens = List(
      Permutation.transposition(3, 0, 1),
      Permutation.fromCycles(3, List(0, 1, 2))
    )
    val group = Nauty.generateGroup(gens)

    group.size shouldBe 6  // |S3| = 3! = 6
  }

  it should "generate D4 from rotation and reflection" in {
    // D4: symmetries of a square
    // Rotation: (0 1 2 3)
    // Reflection: (0 2)
    val rotation = Permutation.fromCycles(4, List(0, 1, 2, 3))
    val reflection = Permutation.transposition(4, 0, 2)
    val group = Nauty.generateGroup(List(rotation, reflection))

    group.size shouldBe 8  // |D4| = 8
  }

  it should "handle identity generator" in {
    val gen = List(Permutation.identity(5))
    val group = Nauty.generateGroup(gen)

    group.size shouldBe 1
    group should contain(Permutation.identity(5))
  }

  it should "handle empty generator list" in {
    val group = Nauty.generateGroup(Nil)
    group shouldBe empty
  }
}

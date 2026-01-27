package com.thatdot.nauty.cypher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Comprehensive tests for Cypher pattern canonicalization.
 * Ported from test_canonicalization.py
 *
 * This validates that the canonicalization is working correctly by:
 * 1. Generating all automorphic variants of a pattern
 * 2. Computing the canonical hash for each variant
 * 3. Verifying all hashes are identical
 */
class CypherCanonicalizationSpec extends AnyFlatSpec with Matchers {

  /**
   * Test that all variants of a pattern produce the same canonical hash.
   */
  def testPattern(pattern: String): (Boolean, Int, String) = {
    val variants = CypherPattern.automorphicPatterns(pattern, includeOriginal = true)

    if (variants.isEmpty) {
      return (false, 0, "")
    }

    val hashes = variants.map(v => v -> CypherPattern.canonicalHash(v)).toMap
    val uniqueHashes = hashes.values.toSet

    val success = uniqueHashes.size == 1
    (success, variants.size, uniqueHashes.headOption.getOrElse(""))
  }

  // Simple patterns
  "Simple edge pattern (a:Person)-[:KNOWS]->(b:Person)" should "have consistent hashes" in {
    val (success, numVariants, _) = testPattern("(a:Person)-[:KNOWS]->(b:Person)")
    success shouldBe true
    numVariants should be >= 1
  }

  "Simple unlabeled pattern (a)-[:LIKES]->(b)" should "have consistent hashes" in {
    val (success, numVariants, _) = testPattern("(a)-[:LIKES]->(b)")
    success shouldBe true
    numVariants should be >= 1
  }

  // Chain patterns
  "Chain pattern (a:Person)-[:KNOWS]->(b:Person)-[:WORKS_AT]->(c:Company)" should "have consistent hashes" in {
    val (success, numVariants, _) = testPattern("(a:Person)-[:KNOWS]->(b:Person)-[:WORKS_AT]->(c:Company)")
    success shouldBe true
  }

  "Long chain (a)-[:X]->(b)-[:Y]->(c)-[:Z]->(d)" should "have consistent hashes" in {
    val (success, numVariants, _) = testPattern("(a)-[:X]->(b)-[:Y]->(c)-[:Z]->(d)")
    success shouldBe true
  }

  // Patterns with incoming edges
  "Pattern with incoming edge (a:Person)-[:KNOWS]->(b:Person)<-[:KNOWS]-(c:Person)" should "have consistent hashes" in {
    val (success, _, _) = testPattern("(a:Person)-[:KNOWS]->(b:Person)<-[:KNOWS]-(c:Person)")
    success shouldBe true
  }

  "Mixed direction pattern (a)<-[:X]-(b)-[:Y]->(c)" should "have consistent hashes" in {
    val (success, _, _) = testPattern("(a)<-[:X]-(b)-[:Y]->(c)")
    success shouldBe true
  }

  // Variable renaming should produce same hash
  "Variable renaming" should "produce same canonical hash" in {
    val patterns = Seq(
      "(a:Person)-[:KNOWS]->(b:Person)",
      "(x:Person)-[:KNOWS]->(y:Person)",
      "(foo:Person)-[:KNOWS]->(bar:Person)",
      "(node1:Person)-[:KNOWS]->(node2:Person)"
    )

    val hashes = patterns.map(CypherPattern.canonicalHash)

    // All should be the same
    hashes.distinct should have size 1
  }

  // Reversed edge syntax should produce same hash
  "Reversed edge syntax" should "produce same canonical hash" in {
    val h1 = CypherPattern.canonicalHash("(a:Person)-[:KNOWS]->(b:Person)")
    val h2 = CypherPattern.canonicalHash("(b:Person)<-[:KNOWS]-(a:Person)")

    h1 shouldBe h2
  }

  // Different labels should produce different hashes
  "Different node labels" should "produce different canonical hashes" in {
    val h1 = CypherPattern.canonicalHash("(a:Person)-[:KNOWS]->(b:Person)")
    val h2 = CypherPattern.canonicalHash("(a:User)-[:KNOWS]->(b:User)")
    val h3 = CypherPattern.canonicalHash("(a:Person)-[:KNOWS]->(b:Company)")

    h1 should not equal h2
    h1 should not equal h3
    h2 should not equal h3
  }

  // Different relationship types should produce different hashes
  "Different relationship types" should "produce different canonical hashes" in {
    val h1 = CypherPattern.canonicalHash("(a:Person)-[:KNOWS]->(b:Person)")
    val h2 = CypherPattern.canonicalHash("(a:Person)-[:LIKES]->(b:Person)")
    val h3 = CypherPattern.canonicalHash("(a:Person)-[:FOLLOWS]->(b:Person)")

    h1 should not equal h2
    h1 should not equal h3
    h2 should not equal h3
  }

  // Different structures should produce different hashes
  "Different graph structures" should "produce different canonical hashes" in {
    // Chain vs single edge
    val hChain = CypherPattern.canonicalHash("(a)-[:X]->(b)-[:X]->(c)")
    val hSingle = CypherPattern.canonicalHash("(a)-[:X]->(b)")

    hChain should not equal hSingle

    // Longer vs shorter chain
    val hLong = CypherPattern.canonicalHash("(a)-[:X]->(b)-[:X]->(c)-[:X]->(d)")
    hLong should not equal hChain
  }

  // Isomorphism detection
  "areIsomorphic" should "correctly detect isomorphic patterns" in {
    CypherPattern.areIsomorphic(
      "(a:Person)-[:KNOWS]->(b:Person)",
      "(x:Person)-[:KNOWS]->(y:Person)"
    ) shouldBe true

    CypherPattern.areIsomorphic(
      "(a:Person)-[:KNOWS]->(b:Person)",
      "(b:Person)<-[:KNOWS]-(a:Person)"
    ) shouldBe true
  }

  it should "correctly detect non-isomorphic patterns" in {
    CypherPattern.areIsomorphic(
      "(a:Person)-[:KNOWS]->(b:Person)",
      "(a:Person)-[:LIKES]->(b:Person)"
    ) shouldBe false

    CypherPattern.areIsomorphic(
      "(a:Person)-[:KNOWS]->(b:Person)",
      "(a:Person)-[:KNOWS]->(b:Company)"
    ) shouldBe false
  }

  // Automorphism group size tests
  "Automorphism group" should "be trivial for asymmetric directed pattern" in {
    val automs = CypherPattern.allAutomorphisms("(a:Person)-[:KNOWS]->(b:Company)")
    automs.size shouldBe 1  // Only identity - different labels on endpoints
  }

  it should "be trivial for directed edge with same labels" in {
    // Even with same labels, directed edge a->b cannot swap endpoints
    val automs = CypherPattern.allAutomorphisms("(a:Person)-[:KNOWS]->(b:Person)")
    automs.size shouldBe 1
  }

  // Edge cases
  "Empty pattern" should "produce EMPTY hash" in {
    val cp = CypherPattern.parse("")
    cp.isEmpty shouldBe true
  }

  "Single node pattern" should "work correctly" in {
    val cp = CypherPattern.parse("(a:Person)")
    cp.nodes shouldBe Map("a" -> "Person")
    cp.edges shouldBe empty

    val hash = CypherPattern.canonicalHash("(a:Person)")
    hash should startWith ("NODES:")
  }

  // Complex patterns from Python test suite
  "Complex chain with cycle back" should "have consistent hashes" in {
    // This is a cycle: a -> b -> c -> a
    val pattern = "(a:Person)-[:KNOWS]->(b:Person)-[:KNOWS]->(c:Person)-[:KNOWS]->(a)"
    val (success, _, _) = testPattern(pattern)
    success shouldBe true
  }

  "Mixed relationship pattern" should "have consistent hashes" in {
    val pattern = "(a:Person)-[:KNOWS]->(b)<-[:BEFRIENDS]-(c:Person)-[:KNOWS]->(a)"
    val (success, _, _) = testPattern(pattern)
    success shouldBe true
  }

  // Test all patterns from test_canonicalization.py
  "All test patterns" should "pass consistency check" in {
    val testPatterns = Seq(
      "(a:Person)-[:KNOWS]->(b:Person)",
      "(a)-[:LIKES]->(b)",
      "(a:Person)-[:KNOWS]->(b:Person)-[:WORKS_AT]->(c:Company)",
      "(a)-[:X]->(b)-[:Y]->(c)-[:Z]->(d)",
      "(a:Person)-[:KNOWS]->(b:Person)<-[:KNOWS]-(c:Person)",
      "(a)<-[:X]-(b)-[:Y]->(c)"
    )

    for (pattern <- testPatterns) {
      withClue(s"Pattern: $pattern") {
        val (success, _, _) = testPattern(pattern)
        success shouldBe true
      }
    }
  }
}

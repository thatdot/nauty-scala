package com.thatdot.nauty.examples

import com.thatdot.nauty.core.Nauty
import com.thatdot.nauty.graph.DenseGraph
import com.thatdot.nauty.util.NautyOptions

/**
 * Example showing how to use scala-nauty for the same tasks as
 * cypher_canonicalize.py and cypher_automorphisms.py.
 */
object CypherExample {

  /**
   * Compute canonical hash for a directed graph with vertex coloring.
   * This is equivalent to what cypher_canonicalize.py does.
   *
   * @param n         Number of vertices
   * @param partition Vertex partition (coloring) - vertices in same cell have same color
   * @param edges     List of directed edges (from, to)
   * @return Canonical hash string
   */
  def canonicalHash(n: Int, partition: Seq[Seq[Int]], edges: Seq[(Int, Int)]): String = {
    val g = DenseGraph.fromEdges(n, edges, directed = true)
    Nauty.canonicalHash(g, partition, directed = true)
  }

  /**
   * Get automorphism generators for a directed graph with vertex coloring.
   * This is equivalent to what cypher_automorphisms.py does to get generators.
   *
   * @param n         Number of vertices
   * @param partition Vertex partition (coloring)
   * @param edges     List of directed edges
   * @return List of generators as cycle strings
   */
  def automorphismGenerators(n: Int, partition: Seq[Seq[Int]], edges: Seq[(Int, Int)]): List[String] = {
    val g = DenseGraph.fromEdges(n, edges, directed = true)
    val generators = Nauty.automorphismGenerators(g, partition, directed = true)
    generators.map(_.toCycleString)
  }

  /**
   * Generate all automorphisms (not just generators).
   * This is equivalent to what cypher_automorphisms.py does to enumerate all automorphisms.
   *
   * @param n         Number of vertices
   * @param partition Vertex partition
   * @param edges     List of directed edges
   * @return Set of all automorphisms as permutation arrays
   */
  def allAutomorphisms(n: Int, partition: Seq[Seq[Int]], edges: Seq[(Int, Int)]): Set[Array[Int]] = {
    val g = DenseGraph.fromEdges(n, edges, directed = true)
    val generators = Nauty.automorphismGenerators(g, partition, directed = true)
    val group = Nauty.generateGroup(generators)
    group.map(_.toArray)
  }

  def main(args: Array[String]): Unit = {
    // Example: A simple directed pattern
    // Node 0: labeled "a"
    // Nodes 1, 2: labeled "b" (equivalent)
    // Edge: 0 -> 1, 0 -> 2

    val n = 3
    val partition = Seq(Seq(0), Seq(1, 2))  // Node 0 alone, nodes 1,2 together
    val edges = Seq((0, 1), (0, 2))

    println("=== Canonical Hash ===")
    val hash = canonicalHash(n, partition, edges)
    println(s"Hash: $hash")

    println("\n=== Automorphism Generators ===")
    val gens = automorphismGenerators(n, partition, edges)
    gens.foreach(g => println(s"  $g"))

    println("\n=== All Automorphisms ===")
    val automs = allAutomorphisms(n, partition, edges)
    println(s"Total: ${automs.size} automorphisms")
    automs.foreach { a =>
      println(s"  ${a.mkString("[", ", ", "]")}")
    }

    // Example 2: Check if two graphs are isomorphic
    println("\n=== Isomorphism Test ===")
    val g1 = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3), (3, 0)))
    val g2 = DenseGraph.fromEdges(4, Seq((0, 2), (2, 1), (1, 3), (3, 0)))
    val isIso = Nauty.isIsomorphic(g1, g2)
    println(s"C4 with different labelings isomorphic: $isIso")
  }
}

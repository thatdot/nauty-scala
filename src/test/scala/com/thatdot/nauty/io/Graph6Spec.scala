package com.thatdot.nauty.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.thatdot.nauty.graph.DenseGraph

class Graph6Spec extends AnyFlatSpec with Matchers {

  "Graph6" should "encode and decode empty graph" in {
    val g = DenseGraph.empty(0)
    val encoded = Graph6.encode(g)
    val decoded = Graph6.decode(encoded)
    decoded.n shouldBe 0
  }

  it should "encode and decode single vertex" in {
    val g = DenseGraph.empty(1)
    val encoded = Graph6.encode(g)
    val decoded = Graph6.decode(encoded)
    decoded.n shouldBe 1
    decoded.numEdges shouldBe 0
  }

  it should "encode and decode K4" in {
    val k4 = DenseGraph.complete(4)
    val encoded = Graph6.encode(k4)
    val decoded = Graph6.decode(encoded)

    decoded.n shouldBe 4
    decoded.numEdges shouldBe 6

    for (i <- 0 until 4; j <- 0 until 4 if i != j) {
      decoded.hasEdge(i, j) shouldBe true
    }
  }

  it should "encode and decode a path graph" in {
    val path = DenseGraph.path(5)
    val encoded = Graph6.encode(path)
    val decoded = Graph6.decode(encoded)

    decoded.n shouldBe 5
    decoded.numEdges shouldBe 4

    decoded.hasEdge(0, 1) shouldBe true
    decoded.hasEdge(1, 2) shouldBe true
    decoded.hasEdge(2, 3) shouldBe true
    decoded.hasEdge(3, 4) shouldBe true
    decoded.hasEdge(0, 4) shouldBe false
  }

  it should "decode known graph6 strings" in {
    // Empty graph on 5 vertices
    val g0 = Graph6.decode("D???")
    g0.n shouldBe 5
    g0.numEdges shouldBe 0

    // K4 (complete graph on 4 vertices)
    // graph6 for K4 is "C~" (C=3 vertices offset by 63=66='B'+1... actually)
    // Let me verify by encoding
    val k4 = DenseGraph.complete(4)
    val k4enc = Graph6.encode(k4)
    val k4dec = Graph6.decode(k4enc)
    k4dec.numEdges shouldBe 6
  }

  it should "round-trip various graphs" in {
    val graphs = Seq(
      DenseGraph.empty(10),
      DenseGraph.complete(5),
      DenseGraph.cycle(6),
      DenseGraph.path(7),
      DenseGraph.fromEdges(8, Seq((0, 7), (1, 6), (2, 5), (3, 4)))
    )

    for (g <- graphs) {
      val encoded = Graph6.encode(g)
      val decoded = Graph6.decode(encoded)

      decoded.n shouldBe g.n
      decoded.numEdges shouldBe g.numEdges

      for (i <- 0 until g.n; j <- 0 until g.n) {
        decoded.hasEdge(i, j) shouldBe g.hasEdge(i, j)
      }
    }
  }

  "Sparse6" should "encode and decode sparse graphs" in {
    val path = DenseGraph.path(10)
    val encoded = Sparse6.encode(path)
    encoded should startWith(":")

    val decoded = Sparse6.decode(encoded)
    decoded.n shouldBe 10
    decoded.numEdges shouldBe 9
  }

  "Digraph6" should "encode and decode directed graphs" in {
    val edges = Seq((0, 1), (1, 2), (2, 0))
    val g = DenseGraph.fromEdges(3, edges, directed = true)

    val encoded = Digraph6.encode(g)
    encoded should startWith("&")

    val decoded = Digraph6.decode(encoded)
    decoded.n shouldBe 3
    decoded.hasEdge(0, 1) shouldBe true
    decoded.hasEdge(1, 0) shouldBe false // directed
  }

  "GraphIO" should "auto-detect format" in {
    val g = DenseGraph.complete(4)

    val g6 = Graph6.encode(g)
    val s6 = Sparse6.encode(g)

    GraphIO.decode(g6).numEdges shouldBe 6
    GraphIO.decode(s6).numEdges shouldBe 6
  }
}

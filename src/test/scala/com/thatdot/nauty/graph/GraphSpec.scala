package com.thatdot.nauty.graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphSpec extends AnyFlatSpec with Matchers {

  "DenseGraph.empty" should "create an empty graph" in {
    val g = DenseGraph.empty(5)
    g.n shouldBe 5
    g.numEdges shouldBe 0
    g.hasEdge(0, 1) shouldBe false
  }

  "DenseGraph.fromEdges" should "create a graph from edge list" in {
    val g = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2), (2, 3)))
    g.n shouldBe 4
    g.numEdges shouldBe 3
    g.hasEdge(0, 1) shouldBe true
    g.hasEdge(1, 0) shouldBe true // undirected
    g.hasEdge(0, 2) shouldBe false
    g.degree(1) shouldBe 2
    g.neighbors(1).toSet shouldBe Set(0, 2)
  }

  "DenseGraph.complete" should "create a complete graph" in {
    val k5 = DenseGraph.complete(5)
    k5.n shouldBe 5
    k5.numEdges shouldBe 10 // 5 choose 2
    k5.hasEdge(0, 4) shouldBe true
    k5.hasEdge(2, 2) shouldBe false // no self-loop
    k5.degree(0) shouldBe 4
  }

  "DenseGraph.cycle" should "create a cycle graph" in {
    val c5 = DenseGraph.cycle(5)
    c5.n shouldBe 5
    c5.numEdges shouldBe 5
    c5.degree(0) shouldBe 2
    c5.hasEdge(0, 1) shouldBe true
    c5.hasEdge(4, 0) shouldBe true
    c5.hasEdge(0, 2) shouldBe false
  }

  "DenseGraph.permute" should "correctly permute a graph" in {
    // Nauty convention: g^p has edge (i,j) iff g has edge (p[i], p[j])
    val g = DenseGraph.fromEdges(4, Seq((0, 1), (1, 2)))
    // Permutation: p[0]=1, p[1]=2, p[2]=3, p[3]=0
    val perm = Array(1, 2, 3, 0)
    val gp = g.permute(perm)

    gp.n shouldBe 4
    gp.numEdges shouldBe 2
    // g has edge (0,1): g^p has edge (i,j) where p[i]=0, p[j]=1 => i=3, j=0, so edge (3,0)
    // g has edge (1,2): g^p has edge (i,j) where p[i]=1, p[j]=2 => i=0, j=1, so edge (0,1)
    gp.hasEdge(3, 0) shouldBe true // from original 0-1
    gp.hasEdge(0, 1) shouldBe true // from original 1-2
    gp.hasEdge(1, 2) shouldBe false
    gp.hasEdge(2, 3) shouldBe false
  }

  "SparseGraph" should "have same behavior as DenseGraph" in {
    val edges = Seq((0, 1), (1, 2), (2, 3))
    val dense = DenseGraph.fromEdges(4, edges)
    val sparse = SparseGraph.fromEdges(4, edges)

    sparse.n shouldBe dense.n
    sparse.numEdges shouldBe dense.numEdges

    for (i <- 0 until 4; j <- 0 until 4) {
      sparse.hasEdge(i, j) shouldBe dense.hasEdge(i, j)
    }
  }

  "Graph conversion" should "preserve structure" in {
    val dense = DenseGraph.fromEdges(5, Seq((0, 1), (1, 2), (2, 3), (3, 4), (4, 0)))
    val sparse = dense.toSparse
    val dense2 = sparse.toDense

    for (i <- 0 until 5; j <- 0 until 5) {
      dense2.hasEdge(i, j) shouldBe dense.hasEdge(i, j)
    }
  }
}

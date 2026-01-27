package com.thatdot.nauty.graph

import com.thatdot.nauty.bits.{SetWord, SetOps}
import scala.collection.mutable.ArrayBuffer

/**
 * Sparse graph representation using compressed sparse row (CSR) format.
 * This matches nauty's sparsegraph structure.
 *
 * For each vertex v:
 *   - v(v) gives the starting index in e
 *   - d(v) gives the out-degree (number of neighbors)
 *   - e(v(v)) .. e(v(v) + d(v) - 1) are the neighbors
 *
 * @param v Array of starting indices (length n)
 * @param d Array of degrees (length n)
 * @param e Array of neighbor vertices (length = sum of degrees)
 * @param n Number of vertices
 * @param directed True if directed graph
 */
final class SparseGraph private(
  private[nauty] val v: Array[Long],   // Index into e for each vertex
  private[nauty] val d: Array[Int],     // Degree of each vertex
  private[nauty] val e: Array[Int],     // Edge endpoints
  val n: Int,
  val directed: Boolean
) extends Graph[SparseGraph] {

  /** Total number of directed edges (each undirected edge counted twice) */
  def nde: Long = e.length.toLong

  override def isDigraph: Boolean = directed

  override def hasEdge(from: Int, to: Int): Boolean = {
    val start = v(from).toInt
    val end = start + d(from)
    var i = start
    while (i < end) {
      if (e(i) == to) return true
      i += 1
    }
    false
  }

  override def neighbors(vertex: Int): IndexedSeq[Int] = {
    val start = v(vertex).toInt
    val end = start + d(vertex)
    e.slice(start, end).toIndexedSeq
  }

  override def degree(vertex: Int): Int = d(vertex)

  override def numEdges: Int = {
    if (directed) e.length
    else e.length / 2
  }

  override def permute(p: Array[Int]): SparseGraph = {
    // Nauty convention: g^p has edge (i,j) iff g has edge (p[i], p[j])
    // So for new vertex i, look at old vertex p[i]'s neighbors,
    // and for each neighbor w, add edge to p^{-1}[w].

    // Build inverse permutation
    val pInv = new Array[Int](n)
    var i = 0
    while (i < n) {
      pInv(p(i)) = i
      i += 1
    }

    val newV = new Array[Long](n)
    val newD = new Array[Int](n)
    val newE = new Array[Int](e.length)

    // First pass: compute new degrees
    // New vertex i has the same degree as old vertex p[i]
    i = 0
    while (i < n) {
      newD(i) = d(p(i))
      i += 1
    }

    // Compute new v array (prefix sums)
    var pos = 0L
    i = 0
    while (i < n) {
      newV(i) = pos
      pos += newD(i)
      i += 1
    }

    // Second pass: fill new edges
    // For new vertex i, copy neighbors of old vertex p[i],
    // mapping each neighbor w to pInv[w]
    i = 0
    while (i < n) {
      val oldVertex = p(i)
      val start = v(oldVertex).toInt
      val deg = d(oldVertex)
      val newStart = newV(i).toInt
      var j = 0
      while (j < deg) {
        newE(newStart + j) = pInv(e(start + j))
        j += 1
      }
      i += 1
    }

    new SparseGraph(newV, newD, newE, n, directed)
  }

  override def toDense: DenseGraph = {
    val m = SetWord.setwordsNeeded(n)
    val data = new Array[Long](n * m)

    var vertex = 0
    while (vertex < n) {
      val start = v(vertex).toInt
      val deg = d(vertex)
      var i = 0
      while (i < deg) {
        val neighbor = e(start + i)
        val wordIdx = neighbor >> 6
        val bitIdx = neighbor & 0x3F
        data(vertex * m + wordIdx) |= SetWord.bit(bitIdx)
        i += 1
      }
      vertex += 1
    }
    DenseGraph.fromData(data, n, m, directed)
  }

  override def toSparse: SparseGraph = this

  override def copy: SparseGraph = {
    new SparseGraph(v.clone(), d.clone(), e.clone(), n, directed)
  }

  /**
   * Sort the adjacency lists for each vertex.
   * This is required before some operations.
   */
  def sorted: SparseGraph = {
    val newE = e.clone()
    var vertex = 0
    while (vertex < n) {
      val start = v(vertex).toInt
      val end = start + d(vertex)
      java.util.Arrays.sort(newE, start, end)
      vertex += 1
    }
    new SparseGraph(v, d, newE, n, directed)
  }

  /**
   * Check if adjacency lists are sorted.
   */
  def isSorted: Boolean = {
    var vertex = 0
    while (vertex < n) {
      val start = v(vertex).toInt
      val end = start + d(vertex)
      var i = start + 1
      while (i < end) {
        if (e(i - 1) > e(i)) return false
        i += 1
      }
      vertex += 1
    }
    true
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: SparseGraph =>
      if (n != other.n || directed != other.directed) return false
      // Compare sorted versions
      val thisSorted = if (isSorted) this else sorted
      val otherSorted = if (other.isSorted) other else other.sorted
      java.util.Arrays.equals(thisSorted.d, otherSorted.d) &&
        java.util.Arrays.equals(thisSorted.e, otherSorted.e)
    case _ => false
  }

  override def hashCode(): Int = {
    val s = if (isSorted) this else sorted
    java.util.Arrays.hashCode(s.e) * 31 + n
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(s"SparseGraph(n=$n, edges=[")
    var first = true
    var vertex = 0
    while (vertex < n) {
      val start = v(vertex).toInt
      val deg = d(vertex)
      var i = 0
      while (i < deg) {
        val neighbor = e(start + i)
        if (directed || vertex <= neighbor) {
          if (!first) sb.append(", ")
          first = false
          if (directed) sb.append(s"$vertex->$neighbor")
          else sb.append(s"$vertex-$neighbor")
        }
        i += 1
      }
      vertex += 1
    }
    sb.append("])")
    sb.toString
  }
}

object SparseGraph {
  /**
   * Create an empty graph with n vertices.
   */
  def empty(n: Int, directed: Boolean = false): SparseGraph = {
    val v = new Array[Long](n)
    val d = new Array[Int](n)
    val e = new Array[Int](0)
    new SparseGraph(v, d, e, n, directed)
  }

  /**
   * Create a graph from edge list.
   */
  def fromEdges(n: Int, edges: Iterable[(Int, Int)], directed: Boolean = false): SparseGraph = {
    val builder = new SparseGraphBuilder(n, edges.size * 2, directed)
    for ((v, w) <- edges) {
      if (directed) {
        builder.addArc(v, w)
      } else {
        builder.addEdge(v, w)
      }
    }
    builder.build()
  }

  /**
   * Create from raw data (for internal use).
   */
  private[nauty] def fromData(
    v: Array[Long],
    d: Array[Int],
    e: Array[Int],
    n: Int,
    directed: Boolean
  ): SparseGraph = {
    new SparseGraph(v.clone(), d.clone(), e.clone(), n, directed)
  }

  /**
   * Create a complete graph K_n.
   */
  def complete(n: Int): SparseGraph = {
    val builder = new SparseGraphBuilder(n, n * (n - 1), false)
    var i = 0
    while (i < n) {
      var j = i + 1
      while (j < n) {
        builder.addEdge(i, j)
        j += 1
      }
      i += 1
    }
    builder.build()
  }

  /**
   * Create a cycle graph C_n.
   */
  def cycle(n: Int): SparseGraph = {
    require(n >= 3, "Cycle requires at least 3 vertices")
    val edges = (0 until n).map(i => (i, (i + 1) % n))
    fromEdges(n, edges)
  }

  /**
   * Create a path graph P_n.
   */
  def path(n: Int): SparseGraph = {
    require(n >= 1, "Path requires at least 1 vertex")
    val edges = (0 until n - 1).map(i => (i, i + 1))
    fromEdges(n, edges)
  }
}

/**
 * Builder for constructing sparse graphs incrementally.
 */
class SparseGraphBuilder(n: Int, estimatedEdges: Int = 0, directed: Boolean = false) {
  private val adjacencyLists: Array[ArrayBuffer[Int]] = Array.fill(n)(new ArrayBuffer[Int]())

  /**
   * Add a directed arc from v to w.
   */
  def addArc(v: Int, w: Int): this.type = {
    require(v >= 0 && v < n && w >= 0 && w < n, s"Invalid arc ($v, $w) for n=$n")
    adjacencyLists(v) += w
    this
  }

  /**
   * Add an undirected edge between v and w.
   */
  def addEdge(v: Int, w: Int): this.type = {
    require(v >= 0 && v < n && w >= 0 && w < n, s"Invalid edge ($v, $w) for n=$n")
    adjacencyLists(v) += w
    if (v != w) {  // Don't add self-loops twice
      adjacencyLists(w) += v
    }
    this
  }

  /**
   * Build the sparse graph.
   */
  def build(): SparseGraph = {
    val vArr = new Array[Long](n)
    val dArr = new Array[Int](n)

    // Compute total edges and prefix sums
    var totalEdges = 0
    var i = 0
    while (i < n) {
      vArr(i) = totalEdges
      dArr(i) = adjacencyLists(i).length
      totalEdges += dArr(i)
      i += 1
    }

    // Fill edge array
    val eArr = new Array[Int](totalEdges)
    i = 0
    while (i < n) {
      val start = vArr(i).toInt
      val adj = adjacencyLists(i)
      var j = 0
      while (j < adj.length) {
        eArr(start + j) = adj(j)
        j += 1
      }
      i += 1
    }

    SparseGraph.fromData(vArr, dArr, eArr, n, directed)
  }
}

package com.thatdot.nauty.graph

import com.thatdot.nauty.bits.{SetWord, BitOps, SetOps}

/**
 * Base trait for graph representations.
 * Graphs are immutable once constructed.
 *
 * @tparam G The concrete graph type (for return type polymorphism)
 */
trait Graph[G <: Graph[G]] { self: G =>
  /** Number of vertices */
  def n: Int

  /** Number of edges (undirected) or arcs (directed) */
  def numEdges: Int

  /** Test if there is an edge/arc from v to w */
  def hasEdge(v: Int, w: Int): Boolean

  /** Get all neighbors of vertex v */
  def neighbors(v: Int): IndexedSeq[Int]

  /** Degree of vertex v (out-degree for digraphs) */
  def degree(v: Int): Int

  /** True if this is a directed graph */
  def isDigraph: Boolean

  /** Apply a permutation to get a relabeled graph: result has edge (p(v), p(w)) iff original has (v, w) */
  def permute(p: Array[Int]): G

  /** Convert to dense representation */
  def toDense: DenseGraph

  /** Convert to sparse representation */
  def toSparse: SparseGraph

  /** Create a copy */
  def copy: G

  /**
   * Compute the girth (length of shortest cycle) of this graph.
   * Returns Int.MaxValue if the graph is acyclic (a forest).
   * For directed graphs, finds the shortest directed cycle.
   */
  def girth: Int = {
    if (n == 0) return Int.MaxValue

    var minCycle = Int.MaxValue

    // For each vertex, do BFS to find shortest cycle through it
    for (start <- 0 until n) {
      val dist = Array.fill(n)(-1)
      val parent = Array.fill(n)(-1)
      val queue = scala.collection.mutable.Queue[Int]()

      dist(start) = 0
      queue.enqueue(start)

      while (queue.nonEmpty && dist(queue.head) < minCycle / 2) {
        val v = queue.dequeue()
        for (w <- neighbors(v)) {
          if (dist(w) == -1) {
            dist(w) = dist(v) + 1
            parent(w) = v
            queue.enqueue(w)
          } else if (parent(v) != w || isDigraph) {
            // Found a cycle: w was already visited and is not v's parent
            // (for undirected graphs, back-edge to parent doesn't count)
            val cycleLen = dist(v) + dist(w) + 1
            minCycle = math.min(minCycle, cycleLen)
          }
        }
      }
    }

    minCycle
  }

  /**
   * Compute the diameter (maximum shortest path length) of this graph.
   * Returns Int.MaxValue if the graph is disconnected.
   * Returns 0 for a single vertex graph.
   */
  def diameter: Int = {
    if (n <= 1) return 0

    var maxDist = 0

    // BFS from each vertex to find max shortest path
    for (start <- 0 until n) {
      val dist = Array.fill(n)(-1)
      val queue = scala.collection.mutable.Queue[Int]()

      dist(start) = 0
      queue.enqueue(start)

      while (queue.nonEmpty) {
        val v = queue.dequeue()
        for (w <- neighbors(v)) {
          if (dist(w) == -1) {
            dist(w) = dist(v) + 1
            maxDist = math.max(maxDist, dist(w))
            queue.enqueue(w)
          }
        }
      }

      // Check if graph is disconnected
      if (dist.contains(-1)) return Int.MaxValue
    }

    maxDist
  }

  /**
   * Compute the radius (minimum eccentricity) of this graph.
   * Returns Int.MaxValue if the graph is disconnected.
   */
  def radius: Int = {
    if (n <= 1) return 0

    var minEcc = Int.MaxValue

    for (start <- 0 until n) {
      val dist = Array.fill(n)(-1)
      val queue = scala.collection.mutable.Queue[Int]()
      var maxDistFromStart = 0

      dist(start) = 0
      queue.enqueue(start)

      while (queue.nonEmpty) {
        val v = queue.dequeue()
        for (w <- neighbors(v)) {
          if (dist(w) == -1) {
            dist(w) = dist(v) + 1
            maxDistFromStart = math.max(maxDistFromStart, dist(w))
            queue.enqueue(w)
          }
        }
      }

      // If disconnected from start, eccentricity is infinite
      if (dist.contains(-1)) return Int.MaxValue

      minEcc = math.min(minEcc, maxDistFromStart)
    }

    minEcc
  }

  /**
   * Check if this graph is connected.
   * For directed graphs, checks weak connectivity (ignoring edge directions).
   */
  def isConnected: Boolean = {
    if (n <= 1) return true

    val visited = Array.fill(n)(false)
    val queue = scala.collection.mutable.Queue[Int]()

    visited(0) = true
    queue.enqueue(0)

    var count = 1
    while (queue.nonEmpty) {
      val v = queue.dequeue()
      for (w <- neighbors(v)) {
        if (!visited(w)) {
          visited(w) = true
          count += 1
          queue.enqueue(w)
        }
      }
    }

    count == n
  }
}

/**
 * Dense graph representation using bit-packed adjacency matrix.
 * This matches nauty's native dense graph format.
 *
 * The graph is stored as n rows, each row is m setwords (64-bit longs).
 * Row i contains the adjacency bits for vertex i.
 * Bit j in row i is set if there's an edge from i to j.
 *
 * @param data The adjacency data: Array of n*m setwords
 * @param n    Number of vertices
 * @param m    Number of setwords per row
 * @param directed True if this is a directed graph
 */
final class DenseGraph private(
  private[nauty] val data: Array[Long],
  val n: Int,
  val m: Int,
  val directed: Boolean
) extends Graph[DenseGraph] {

  override def isDigraph: Boolean = directed

  override def hasEdge(v: Int, w: Int): Boolean = {
    SetOps.isElement(graphRow(v), w)
  }

  override def neighbors(v: Int): IndexedSeq[Int] = {
    val row = graphRow(v)
    val result = new scala.collection.mutable.ArrayBuffer[Int](degree(v))
    var pos = -1
    pos = SetOps.nextElement(row, m, pos)
    while (pos >= 0 && pos < n) {
      result += pos
      pos = SetOps.nextElement(row, m, pos)
    }
    result.toIndexedSeq
  }

  override def degree(v: Int): Int = {
    SetOps.setSize(graphRow(v), m)
  }

  override def numEdges: Int = {
    var count = 0
    var v = 0
    while (v < n) {
      count += degree(v)
      v += 1
    }
    if (directed) count else count / 2
  }

  /**
   * Get a view of row v's adjacency data.
   */
  private[nauty] def graphRow(v: Int): Array[Long] = {
    val start = v * m
    val row = new Array[Long](m)
    System.arraycopy(data, start, row, 0, m)
    row
  }

  /**
   * Get the setword at position (v, w/64).
   */
  private[nauty] def getWord(v: Int, wordIdx: Int): Long = {
    data(v * m + wordIdx)
  }

  override def permute(p: Array[Int]): DenseGraph = {
    // Nauty convention: g^p has edge (i,j) iff g has edge (p[i], p[j])
    // So for new vertex i, we look at old vertex p[i]'s neighbors,
    // and for each neighbor w, we set bit at p^{-1}[w] in new row i.

    // Build inverse permutation
    val pInv = new Array[Int](n)
    var i = 0
    while (i < n) {
      pInv(p(i)) = i
      i += 1
    }

    val newData = new Array[Long](n * m)
    val work = new Array[Long](m)

    // For each vertex i in the new graph
    i = 0
    while (i < n) {
      // Clear work array
      SetOps.emptySet(work, m)
      // Look at row p[i] in the old graph
      val row = graphRow(p(i))
      var pos = -1
      pos = SetOps.nextElement(row, m, pos)
      while (pos >= 0 && pos < n) {
        // For each neighbor pos of p[i], set bit pInv[pos] in new row i
        SetOps.addElement(work, pInv(pos))
        pos = SetOps.nextElement(row, m, pos)
      }
      // Copy work to row i of newData
      System.arraycopy(work, 0, newData, i * m, m)
      i += 1
    }
    new DenseGraph(newData, n, m, directed)
  }

  override def toDense: DenseGraph = this

  override def toSparse: SparseGraph = {
    val builder = new SparseGraphBuilder(n, numEdges, directed)
    var v = 0
    while (v < n) {
      val row = graphRow(v)
      var pos = -1
      pos = SetOps.nextElement(row, m, pos)
      while (pos >= 0 && pos < n) {
        builder.addArc(v, pos)
        pos = SetOps.nextElement(row, m, pos)
      }
      v += 1
    }
    builder.build()
  }

  override def copy: DenseGraph = {
    new DenseGraph(data.clone(), n, m, directed)
  }

  /**
   * Compare this graph with another for canonical ordering.
   * Returns negative if this < other, 0 if equal, positive if this > other.
   */
  def compare(other: DenseGraph): Int = {
    require(n == other.n && m == other.m)
    var i = 0
    val len = n * m
    while (i < len) {
      // Compare as unsigned longs
      val a = data(i)
      val b = other.data(i)
      if (a != b) {
        return java.lang.Long.compareUnsigned(a, b)
      }
      i += 1
    }
    0
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: DenseGraph =>
      n == other.n && m == other.m && java.util.Arrays.equals(data, other.data)
    case _ => false
  }

  override def hashCode(): Int = {
    java.util.Arrays.hashCode(data) * 31 + n
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(s"DenseGraph(n=$n, edges=[")
    var first = true
    var v = 0
    while (v < n) {
      val row = graphRow(v)
      var pos = -1
      pos = SetOps.nextElement(row, m, pos)
      while (pos >= 0 && pos < n) {
        if (!directed || v <= pos || !hasEdge(pos, v)) {
          if (!first) sb.append(", ")
          first = false
          if (directed) sb.append(s"$v->$pos")
          else sb.append(s"$v-$pos")
        }
        pos = SetOps.nextElement(row, m, pos)
      }
      v += 1
    }
    sb.append("])")
    sb.toString
  }
}

object DenseGraph {
  /**
   * Create an empty graph with n vertices.
   */
  def empty(n: Int, directed: Boolean = false): DenseGraph = {
    val m = SetWord.setwordsNeeded(n)
    val data = new Array[Long](n * m)
    new DenseGraph(data, n, m, directed)
  }

  /**
   * Create a graph from edge list.
   */
  def fromEdges(n: Int, edges: Iterable[(Int, Int)], directed: Boolean = false): DenseGraph = {
    val m = SetWord.setwordsNeeded(n)
    val data = new Array[Long](n * m)

    for ((v, w) <- edges) {
      require(v >= 0 && v < n && w >= 0 && w < n, s"Invalid edge ($v, $w) for n=$n")
      // Add edge v -> w
      data(v * m + (w >> 6)) |= SetWord.bit(w & 0x3F)
      if (!directed) {
        // Add edge w -> v
        data(w * m + (v >> 6)) |= SetWord.bit(v & 0x3F)
      }
    }
    new DenseGraph(data, n, m, directed)
  }

  /**
   * Create a complete graph K_n.
   */
  def complete(n: Int): DenseGraph = {
    val m = SetWord.setwordsNeeded(n)
    val data = new Array[Long](n * m)

    // Fill each row with bits for all vertices except self
    var v = 0
    while (v < n) {
      val start = v * m
      var i = 0
      val fullWords = n / 64
      while (i < fullWords) {
        data(start + i) = SetWord.AllBits
        i += 1
      }
      val remainder = n - fullWords * 64
      if (remainder > 0) {
        data(start + i) = BitOps.allmask(remainder)
        i += 1
      }
      while (i < m) {
        data(start + i) = 0L
        i += 1
      }
      // Clear self-loop
      data(start + (v >> 6)) &= ~SetWord.bit(v & 0x3F)
      v += 1
    }
    new DenseGraph(data, n, m, false)
  }

  /**
   * Create a cycle graph C_n.
   */
  def cycle(n: Int): DenseGraph = {
    require(n >= 3, "Cycle requires at least 3 vertices")
    val edges = (0 until n).map(i => (i, (i + 1) % n))
    fromEdges(n, edges)
  }

  /**
   * Create a path graph P_n.
   */
  def path(n: Int): DenseGraph = {
    require(n >= 1, "Path requires at least 1 vertex")
    val edges = (0 until n - 1).map(i => (i, i + 1))
    fromEdges(n, edges)
  }

  /**
   * Create from raw data (for internal use).
   */
  private[nauty] def fromData(data: Array[Long], n: Int, m: Int, directed: Boolean): DenseGraph = {
    new DenseGraph(data.clone(), n, m, directed)
  }
}

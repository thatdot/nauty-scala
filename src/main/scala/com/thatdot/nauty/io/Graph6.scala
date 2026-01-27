package com.thatdot.nauty.io

import com.thatdot.nauty.graph.{DenseGraph, SparseGraph, Graph}
import com.thatdot.nauty.bits.{SetWord, SetOps}

/**
 * Graph6 encoding and decoding for undirected graphs.
 *
 * Graph6 is a compact ASCII encoding for undirected graphs.
 * Format: N(n) R(x) where:
 * - N(n) encodes the number of vertices
 * - R(x) encodes the upper triangle of the adjacency matrix
 *
 * Each byte is biased by 63 ('?') to make it printable ASCII.
 */
object Graph6 {
  private val Bias = 63
  private val SmallN = 62
  private val SmallishN = 258047

  /**
   * Encode a graph to graph6 format.
   */
  def encode(g: Graph[_]): String = {
    val n = g.n
    val sb = new StringBuilder

    // Encode n
    encodeN(sb, n)

    // Encode adjacency matrix (upper triangle, row by row)
    var currentByte = 0
    var bitCount = 0

    var j = 1
    while (j < n) {
      var i = 0
      while (i < j) {
        currentByte = (currentByte << 1) | (if (g.hasEdge(i, j)) 1 else 0)
        bitCount += 1

        if (bitCount == 6) {
          sb.append((currentByte + Bias).toChar)
          currentByte = 0
          bitCount = 0
        }
        i += 1
      }
      j += 1
    }

    // Pad final byte if needed
    if (bitCount > 0) {
      currentByte = currentByte << (6 - bitCount)
      sb.append((currentByte + Bias).toChar)
    }

    sb.toString()
  }

  /**
   * Decode a graph6 string to a DenseGraph.
   */
  def decode(s: String): DenseGraph = {
    var pos = 0

    // Skip header if present
    if (s.startsWith(">>graph6<<")) {
      pos = 10
    }

    // Decode n
    val (n, newPos) = decodeN(s, pos)
    pos = newPos

    // Create graph
    val m = SetWord.setwordsNeeded(n)
    val data = new Array[Long](n * m)

    // Decode adjacency matrix
    var currentByte = 0
    var bitsLeft = 0

    var j = 1
    while (j < n) {
      var i = 0
      while (i < j) {
        if (bitsLeft == 0) {
          if (pos < s.length) {
            currentByte = s.charAt(pos) - Bias
            pos += 1
          } else {
            currentByte = 0
          }
          bitsLeft = 6
        }

        bitsLeft -= 1
        val bit = (currentByte >> bitsLeft) & 1

        if (bit == 1) {
          // Add edge i-j (both directions for undirected)
          data(i * m + (j >> 6)) |= SetWord.bit(j & 0x3F)
          data(j * m + (i >> 6)) |= SetWord.bit(i & 0x3F)
        }
        i += 1
      }
      j += 1
    }

    DenseGraph.fromData(data, n, m, directed = false)
  }

  /**
   * Encode the number of vertices.
   */
  private def encodeN(sb: StringBuilder, n: Int): Unit = {
    if (n <= SmallN) {
      sb.append((n + Bias).toChar)
    } else if (n <= SmallishN) {
      sb.append((126).toChar) // 126 = 63 + 63
      sb.append(((n >> 12) + Bias).toChar)
      sb.append((((n >> 6) & 0x3F) + Bias).toChar)
      sb.append(((n & 0x3F) + Bias).toChar)
    } else {
      sb.append((126).toChar)
      sb.append((126).toChar)
      sb.append(((n >> 30) + Bias).toChar)
      sb.append((((n >> 24) & 0x3F) + Bias).toChar)
      sb.append((((n >> 18) & 0x3F) + Bias).toChar)
      sb.append((((n >> 12) & 0x3F) + Bias).toChar)
      sb.append((((n >> 6) & 0x3F) + Bias).toChar)
      sb.append(((n & 0x3F) + Bias).toChar)
    }
  }

  /**
   * Decode the number of vertices.
   * Returns (n, newPosition).
   */
  private def decodeN(s: String, pos: Int): (Int, Int) = {
    val c = s.charAt(pos) - Bias
    if (c < 63) {
      (c, pos + 1)
    } else if (s.charAt(pos + 1) - Bias < 63) {
      val n = ((s.charAt(pos + 1) - Bias) << 12) |
              ((s.charAt(pos + 2) - Bias) << 6) |
              (s.charAt(pos + 3) - Bias)
      (n, pos + 4)
    } else {
      val n = ((s.charAt(pos + 2) - Bias) << 30) |
              ((s.charAt(pos + 3) - Bias) << 24) |
              ((s.charAt(pos + 4) - Bias) << 18) |
              ((s.charAt(pos + 5) - Bias) << 12) |
              ((s.charAt(pos + 6) - Bias) << 6) |
              (s.charAt(pos + 7) - Bias)
      (n, pos + 8)
    }
  }

  /**
   * Check if a string looks like graph6 format.
   */
  def isGraph6(s: String): Boolean = {
    if (s.isEmpty) return false
    val start = if (s.startsWith(">>graph6<<")) 10 else 0
    if (start >= s.length) return false

    val firstChar = s.charAt(start)
    // Graph6 strings don't start with ':' (sparse6) or '&' (digraph6)
    firstChar != ':' && firstChar != '&' && firstChar >= '?' && firstChar <= '~'
  }
}

/**
 * Sparse6 encoding and decoding for sparse undirected graphs.
 *
 * Sparse6 is more compact than graph6 for sparse graphs.
 * Format: ':' N(n) R(x) where x encodes edges using variable-length encoding.
 */
object Sparse6 {
  private val Bias = 63

  /**
   * Encode a graph to sparse6 format.
   */
  def encode(g: Graph[_]): String = {
    val n = g.n
    val sb = new StringBuilder
    sb.append(':')

    // Encode n
    encodeN(sb, n)

    if (n == 0) return sb.toString()

    // Number of bits needed to represent a vertex
    val k = if (n <= 1) 1 else 32 - Integer.numberOfLeadingZeros(n - 1)

    var currentBits = 0
    var numBits = 0

    def writeBit(bit: Int): Unit = {
      currentBits = (currentBits << 1) | bit
      numBits += 1
      if (numBits == 6) {
        sb.append((currentBits + Bias).toChar)
        currentBits = 0
        numBits = 0
      }
    }

    def writeVertex(v: Int): Unit = {
      var i = k - 1
      while (i >= 0) {
        writeBit((v >> i) & 1)
        i -= 1
      }
    }

    // Encode edges
    var v = 0
    var j = 0
    while (j < n) {
      var i = 0
      while (i < j) {
        if (g.hasEdge(i, j)) {
          if (j == v) {
            writeBit(0)
          } else if (j == v + 1) {
            writeBit(1)
            writeBit(0)
            v = j
          } else {
            writeBit(1)
            writeBit(1)
            writeVertex(j)
            v = j
            writeBit(0)
          }
          writeVertex(i)
        }
        i += 1
      }
      j += 1
    }

    // Pad to byte boundary
    if (numBits > 0) {
      // Pad with 1s if remaining bits would cause a spurious edge
      val padBit = if (numBits <= k && n <= ((1 << (6 - numBits)) - 1)) 1 else 0
      while (numBits < 6) {
        currentBits = (currentBits << 1) | padBit
        numBits += 1
      }
      sb.append((currentBits + Bias).toChar)
    }

    sb.toString()
  }

  /**
   * Decode a sparse6 string to a DenseGraph.
   */
  def decode(s: String): DenseGraph = {
    var pos = 0

    // Skip header if present
    if (s.startsWith(">>sparse6<<")) {
      pos = 11
    }

    // Skip ':' prefix
    if (pos < s.length && s.charAt(pos) == ':') {
      pos += 1
    }

    // Decode n
    val (n, newPos) = decodeN(s, pos)
    pos = newPos

    if (n == 0) return DenseGraph.empty(0)

    // Number of bits to represent a vertex
    val k = if (n <= 1) 1 else 32 - Integer.numberOfLeadingZeros(n - 1)

    val m = SetWord.setwordsNeeded(n)
    val data = new Array[Long](n * m)

    var currentByte = 0
    var bitsLeft = 0

    def readBit(): Int = {
      if (bitsLeft == 0) {
        if (pos < s.length) {
          currentByte = s.charAt(pos) - Bias
          pos += 1
        } else {
          currentByte = 0
        }
        bitsLeft = 6
      }
      bitsLeft -= 1
      (currentByte >> bitsLeft) & 1
    }

    def readVertex(): Int = {
      var v = 0
      var i = 0
      while (i < k) {
        v = (v << 1) | readBit()
        i += 1
      }
      v
    }

    var v = 0
    while (pos < s.length || bitsLeft > k) {
      val b = readBit()
      if (b == 1) {
        v += 1
        if (readBit() == 1) {
          v = readVertex()
          if (v >= n) return DenseGraph.fromData(data, n, m, directed = false)
        }
      }
      val x = readVertex()
      if (x >= n || v >= n) return DenseGraph.fromData(data, n, m, directed = false)

      if (x < v) {
        // Add edge x-v
        data(x * m + (v >> 6)) |= SetWord.bit(v & 0x3F)
        data(v * m + (x >> 6)) |= SetWord.bit(x & 0x3F)
      }
    }

    DenseGraph.fromData(data, n, m, directed = false)
  }

  private def encodeN(sb: StringBuilder, n: Int): Unit = {
    val SmallN = 62
    val SmallishN = 258047
    val Bias = 63

    if (n <= SmallN) {
      sb.append((n + Bias).toChar)
    } else if (n <= SmallishN) {
      sb.append((126).toChar)
      sb.append(((n >> 12) + Bias).toChar)
      sb.append((((n >> 6) & 0x3F) + Bias).toChar)
      sb.append(((n & 0x3F) + Bias).toChar)
    } else {
      sb.append((126).toChar)
      sb.append((126).toChar)
      sb.append(((n >> 30) + Bias).toChar)
      sb.append((((n >> 24) & 0x3F) + Bias).toChar)
      sb.append((((n >> 18) & 0x3F) + Bias).toChar)
      sb.append((((n >> 12) & 0x3F) + Bias).toChar)
      sb.append((((n >> 6) & 0x3F) + Bias).toChar)
      sb.append(((n & 0x3F) + Bias).toChar)
    }
  }

  private def decodeN(s: String, pos: Int): (Int, Int) = {
    val Bias = 63
    val c = s.charAt(pos) - Bias
    if (c < 63) {
      (c, pos + 1)
    } else if (pos + 1 < s.length && s.charAt(pos + 1) - Bias < 63) {
      val n = ((s.charAt(pos + 1) - Bias) << 12) |
              ((s.charAt(pos + 2) - Bias) << 6) |
              (s.charAt(pos + 3) - Bias)
      (n, pos + 4)
    } else {
      val n = ((s.charAt(pos + 2) - Bias) << 30) |
              ((s.charAt(pos + 3) - Bias) << 24) |
              ((s.charAt(pos + 4) - Bias) << 18) |
              ((s.charAt(pos + 5) - Bias) << 12) |
              ((s.charAt(pos + 6) - Bias) << 6) |
              (s.charAt(pos + 7) - Bias)
      (n, pos + 8)
    }
  }

  /**
   * Check if a string looks like sparse6 format.
   */
  def isSparse6(s: String): Boolean = {
    if (s.isEmpty) return false
    if (s.startsWith(">>sparse6<<")) return true
    s.charAt(0) == ':'
  }
}

/**
 * Digraph6 encoding and decoding for directed graphs.
 */
object Digraph6 {
  private val Bias = 63

  /**
   * Encode a directed graph to digraph6 format.
   */
  def encode(g: Graph[_]): String = {
    require(g.isDigraph, "Digraph6 requires a directed graph")
    val n = g.n
    val sb = new StringBuilder
    sb.append('&')

    // Encode n
    encodeN(sb, n)

    // Encode full adjacency matrix (all n^2 entries, row by row)
    var currentByte = 0
    var bitCount = 0

    var i = 0
    while (i < n) {
      var j = 0
      while (j < n) {
        currentByte = (currentByte << 1) | (if (g.hasEdge(i, j)) 1 else 0)
        bitCount += 1

        if (bitCount == 6) {
          sb.append((currentByte + Bias).toChar)
          currentByte = 0
          bitCount = 0
        }
        j += 1
      }
      i += 1
    }

    // Pad final byte if needed
    if (bitCount > 0) {
      currentByte = currentByte << (6 - bitCount)
      sb.append((currentByte + Bias).toChar)
    }

    sb.toString()
  }

  /**
   * Decode a digraph6 string to a DenseGraph.
   */
  def decode(s: String): DenseGraph = {
    var pos = 0

    // Skip header if present
    if (s.startsWith(">>digraph6<<")) {
      pos = 12
    }

    // Skip '&' prefix
    if (pos < s.length && s.charAt(pos) == '&') {
      pos += 1
    }

    // Decode n
    val (n, newPos) = decodeN(s, pos)
    pos = newPos

    val m = SetWord.setwordsNeeded(n)
    val data = new Array[Long](n * m)

    // Decode full adjacency matrix
    var currentByte = 0
    var bitsLeft = 0

    var i = 0
    while (i < n) {
      var j = 0
      while (j < n) {
        if (bitsLeft == 0) {
          if (pos < s.length) {
            currentByte = s.charAt(pos) - Bias
            pos += 1
          } else {
            currentByte = 0
          }
          bitsLeft = 6
        }

        bitsLeft -= 1
        val bit = (currentByte >> bitsLeft) & 1

        if (bit == 1) {
          data(i * m + (j >> 6)) |= SetWord.bit(j & 0x3F)
        }
        j += 1
      }
      i += 1
    }

    DenseGraph.fromData(data, n, m, directed = true)
  }

  private def encodeN(sb: StringBuilder, n: Int): Unit = {
    val SmallN = 62
    val SmallishN = 258047

    if (n <= SmallN) {
      sb.append((n + Bias).toChar)
    } else if (n <= SmallishN) {
      sb.append((126).toChar)
      sb.append(((n >> 12) + Bias).toChar)
      sb.append((((n >> 6) & 0x3F) + Bias).toChar)
      sb.append(((n & 0x3F) + Bias).toChar)
    } else {
      sb.append((126).toChar)
      sb.append((126).toChar)
      sb.append(((n >> 30) + Bias).toChar)
      sb.append((((n >> 24) & 0x3F) + Bias).toChar)
      sb.append((((n >> 18) & 0x3F) + Bias).toChar)
      sb.append((((n >> 12) & 0x3F) + Bias).toChar)
      sb.append((((n >> 6) & 0x3F) + Bias).toChar)
      sb.append(((n & 0x3F) + Bias).toChar)
    }
  }

  private def decodeN(s: String, pos: Int): (Int, Int) = {
    val c = s.charAt(pos) - Bias
    if (c < 63) {
      (c, pos + 1)
    } else if (pos + 1 < s.length && s.charAt(pos + 1) - Bias < 63) {
      val n = ((s.charAt(pos + 1) - Bias) << 12) |
              ((s.charAt(pos + 2) - Bias) << 6) |
              (s.charAt(pos + 3) - Bias)
      (n, pos + 4)
    } else {
      val n = ((s.charAt(pos + 2) - Bias) << 30) |
              ((s.charAt(pos + 3) - Bias) << 24) |
              ((s.charAt(pos + 4) - Bias) << 18) |
              ((s.charAt(pos + 5) - Bias) << 12) |
              ((s.charAt(pos + 6) - Bias) << 6) |
              (s.charAt(pos + 7) - Bias)
      (n, pos + 8)
    }
  }

  /**
   * Check if a string looks like digraph6 format.
   */
  def isDigraph6(s: String): Boolean = {
    if (s.isEmpty) return false
    if (s.startsWith(">>digraph6<<")) return true
    s.charAt(0) == '&'
  }
}

/**
 * Unified graph I/O with format auto-detection.
 */
object GraphIO {
  /**
   * Decode a graph from any supported format.
   */
  def decode(s: String): DenseGraph = {
    val trimmed = s.trim
    if (Digraph6.isDigraph6(trimmed)) {
      Digraph6.decode(trimmed)
    } else if (Sparse6.isSparse6(trimmed)) {
      Sparse6.decode(trimmed)
    } else {
      Graph6.decode(trimmed)
    }
  }

  /**
   * Encode a graph, choosing format based on density.
   */
  def encode(g: Graph[_], preferSparse: Boolean = false): String = {
    if (g.isDigraph) {
      Digraph6.encode(g)
    } else if (preferSparse) {
      Sparse6.encode(g)
    } else {
      Graph6.encode(g)
    }
  }
}

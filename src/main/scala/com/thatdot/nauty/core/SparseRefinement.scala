package com.thatdot.nauty.core

import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.bits.SetOps

/**
 * Sparse graph refinement operations.
 *
 * This implements partition refinement directly on sparse graphs without
 * converting to dense representation, matching nauty's nausparse.c.
 */
object SparseRefinement {

  /**
   * Refine a partition based on adjacency to an active cell.
   * Works directly with sparse adjacency lists.
   *
   * @param g        The sparse graph
   * @param lab      Current labeling (modified in place)
   * @param ptn      Current partition markers (modified in place)
   * @param level    Current level (cells end where ptn(i) <= level)
   * @param numCells Current number of cells (updated)
   * @param active   Active cells to refine against
   * @param m        Number of setwords per row (for active set)
   * @param n        Number of vertices
   * @return A hash code for the refinement
   */
  def refine(
    g: SparseGraph,
    lab: Array[Int],
    ptn: Array[Int],
    level: Int,
    numCells: IntRef,
    active: Array[Long],
    m: Int,
    n: Int
  ): Int = {
    if (n == 0) return 0

    // Work arrays
    val count = new Array[Int](n)      // neighbor count for each vertex
    val cellStart = new Array[Int](n)  // start of cell containing each vertex
    val bucket = new Array[Int](n + 2) // bucket sort workspace
    val workActive = new Array[Int](n) // active cell queue

    var longcode: Long = numCells.value

    // Build active cell queue
    var nactive = 0
    var pos = -1
    pos = SetOps.nextElement(active, m, pos)
    while (pos >= 0 && pos < n) {
      workActive(nactive) = pos
      nactive += 1
      pos = SetOps.nextElement(active, m, pos)
    }

    if (nactive == 0) {
      return (longcode % 077777).toInt
    }

    // Set cellStart[v] = starting position of v's cell in lab, or n if singleton
    var i = 0
    while (i < n) {
      if (ptn(i) <= level) {
        // Singleton cell
        cellStart(lab(i)) = n
        i += 1
      } else {
        // Non-singleton cell starting at i
        val start = i
        while (i < n && (i == start || ptn(i - 1) > level)) {
          cellStart(lab(i)) = start
          i += 1
        }
      }
    }

    // Process each active cell
    var activeIdx = 0
    while (activeIdx < nactive) {
      val splitCell = workActive(activeIdx)
      activeIdx += 1
      SetOps.delElement(active, splitCell)

      // Count neighbors in active cell for each vertex
      java.util.Arrays.fill(count, 0)

      // Find bounds of the active cell
      val cellEnd = {
        var j = splitCell
        while (j < n - 1 && ptn(j) > level) j += 1
        j + 1
      }

      // For each vertex in the active cell, increment count of its neighbors
      var j = splitCell
      while (j < cellEnd) {
        val v = lab(j)
        val neighbors = g.neighbors(v)
        var k = 0
        while (k < neighbors.length) {
          count(neighbors(k)) += 1
          k += 1
        }
        j += 1
      }

      longcode = mash(longcode, splitCell)

      // Now split non-singleton cells based on count values
      i = 0
      while (i < n) {
        if (ptn(i) <= level) {
          // Singleton - skip
          i += 1
        } else {
          // Non-singleton cell starting at i
          val cellStartPos = i

          // Find cell end
          var cellEndPos = i
          while (cellEndPos < n - 1 && ptn(cellEndPos) > level) {
            cellEndPos += 1
          }
          cellEndPos += 1

          // Check if all vertices in cell have same count
          val firstCount = count(lab(cellStartPos))
          var allSame = true
          var checkIdx = cellStartPos + 1
          while (checkIdx < cellEndPos && allSame) {
            if (count(lab(checkIdx)) != firstCount) allSame = false
            checkIdx += 1
          }

          if (allSame) {
            // No split needed
            longcode = mash(longcode, firstCount)
            i = cellEndPos
          } else {
            // Need to split - use bucket sort
            val splitResult = bucketSortSplit(
              lab, ptn, count, bucket, cellStartPos, cellEndPos, level, active, workActive, nactive
            )
            nactive = splitResult._1
            val newCells = splitResult._2
            numCells.value += newCells
            longcode = mash(longcode, cellStartPos)
            longcode = mash(longcode, newCells)

            // Update cellStart for vertices in new cells
            var updateIdx = cellStartPos
            while (updateIdx < cellEndPos) {
              if (updateIdx == cellStartPos || ptn(updateIdx - 1) <= level) {
                // Start of a new cell
                val newCellStart = updateIdx
                while (updateIdx < cellEndPos && (updateIdx == newCellStart || ptn(updateIdx - 1) > level)) {
                  cellStart(lab(updateIdx)) = if (ptn(updateIdx) <= level) n else newCellStart
                  updateIdx += 1
                }
              }
            }

            i = cellEndPos
          }
        }
      }
    }

    (longcode % 077777).toInt
  }

  /**
   * Bucket sort split of a cell based on count values.
   * Returns (new nactive, number of new cells created).
   */
  private def bucketSortSplit(
    lab: Array[Int],
    ptn: Array[Int],
    count: Array[Int],
    bucket: Array[Int],
    cellStart: Int,
    cellEnd: Int,
    level: Int,
    active: Array[Long],
    workActive: Array[Int],
    nactive: Int
  ): (Int, Int) = {
    val cellSize = cellEnd - cellStart

    // Find min and max count values
    var minCount = count(lab(cellStart))
    var maxCount = minCount
    var i = cellStart + 1
    while (i < cellEnd) {
      val c = count(lab(i))
      if (c < minCount) minCount = c
      if (c > maxCount) maxCount = c
      i += 1
    }

    if (minCount == maxCount) {
      // All same count - no split
      return (nactive, 0)
    }

    val range = maxCount - minCount + 1

    // Count occurrences of each count value
    java.util.Arrays.fill(bucket, 0, range + 1, 0)
    i = cellStart
    while (i < cellEnd) {
      bucket(count(lab(i)) - minCount) += 1
      i += 1
    }

    // Convert to starting positions (prefix sum)
    var sum = 0
    i = 0
    while (i < range) {
      val tmp = bucket(i)
      bucket(i) = sum
      sum += tmp
      i += 1
    }
    bucket(range) = sum

    // Sort into temporary positions
    val sorted = new Array[Int](cellSize)
    i = cellStart
    while (i < cellEnd) {
      val v = lab(i)
      val idx = count(v) - minCount
      sorted(bucket(idx)) = v
      bucket(idx) += 1
      i += 1
    }

    // Copy back to lab
    System.arraycopy(sorted, 0, lab, cellStart, cellSize)

    // Update ptn to mark cell boundaries
    var newCells = 0
    var newActive = nactive

    // Reset bucket to hold starting positions again
    sum = 0
    i = 0
    while (i < range) {
      val count_i = bucket(i) - sum
      bucket(i) = sum
      sum += count_i
      i += 1
    }

    // Mark cell boundaries in ptn
    i = 0
    while (i < range) {
      val start = bucket(i)
      val end = if (i + 1 < range) bucket(i + 1) else cellSize

      if (end > start) {
        // This count value has at least one vertex
        if (start > 0) {
          // Mark boundary
          ptn(cellStart + start - 1) = level
          newCells += 1

          // Add to active if smaller than other fragment
          val prevSize = start
          val thisSize = end - start
          if (thisSize <= prevSize && !SetOps.isElement(active, cellStart + start)) {
            SetOps.addElement(active, cellStart + start)
            workActive(newActive) = cellStart + start
            newActive += 1
          } else if (prevSize < thisSize && !SetOps.isElement(active, cellStart)) {
            SetOps.addElement(active, cellStart)
            // cellStart might already be in workActive, so don't add twice
          }
        }

        // Mark end of cell
        if (end < cellSize) {
          // Will be handled by next iteration
        } else {
          // Last cell - ptn already correct
        }
      }
      i += 1
    }

    (newActive, newCells)
  }

  /**
   * Test if a permutation is an automorphism of a sparse graph.
   * Directly works with adjacency lists.
   */
  def isAutomorphism(g: SparseGraph, perm: Array[Int], digraph: Boolean): Boolean = {
    val n = g.n
    val marked = new Array[Boolean](n)

    var i = 0
    while (i < n) {
      if (perm(i) != i || digraph) {
        val pi = perm(i)
        val di = g.degree(i)

        // Check degrees match
        if (g.degree(pi) != di) return false

        // Mark permuted neighbors of i
        java.util.Arrays.fill(marked, false)
        val neighborsI = g.neighbors(i)
        var j = 0
        while (j < di) {
          marked(perm(neighborsI(j))) = true
          j += 1
        }

        // Check all neighbors of pi are marked
        val neighborsPi = g.neighbors(pi)
        j = 0
        while (j < di) {
          if (!marked(neighborsPi(j))) return false
          j += 1
        }
      }
      i += 1
    }
    true
  }

  /**
   * Compare g^lab with canonical graph canong.
   * Returns (comparison result, number of same rows).
   * comparison: -1 if g^lab < canong, 0 if equal, 1 if greater
   */
  def testCanonLab(
    g: SparseGraph,
    canong: SparseGraph,
    lab: Array[Int]
  ): (Int, Int) = {
    val n = g.n
    val invLab = new Array[Int](n)
    var i = 0
    while (i < n) {
      invLab(lab(i)) = i
      i += 1
    }

    val marked = new Array[Boolean](n)

    i = 0
    while (i < n) {
      // Compare g[lab[i]]^invLab to canong[i]
      val di = canong.degree(i)
      val dli = g.degree(lab(i))

      if (di != dli) {
        return (if (di < dli) -1 else 1, i)
      }

      // Mark canonical neighbors
      java.util.Arrays.fill(marked, false)
      var minA = n
      val canonNeighbors = canong.neighbors(i)
      var j = 0
      while (j < di) {
        marked(canonNeighbors(j)) = true
        j += 1
      }

      // Check permuted neighbors
      val gNeighbors = g.neighbors(lab(i))
      j = 0
      while (j < di) {
        val k = invLab(gNeighbors(j))
        if (marked(k)) {
          marked(k) = false
        } else if (k < minA) {
          minA = k
        }
        j += 1
      }

      if (minA != n) {
        // Found a difference
        j = 0
        while (j < di) {
          val k = canonNeighbors(j)
          if (marked(k) && k < minA) return (-1, i)
          j += 1
        }
        return (1, i)
      }

      i += 1
    }

    (0, n)
  }

  /**
   * Update canonical graph: canong = g^lab.
   */
  def updateCanon(
    g: SparseGraph,
    lab: Array[Int],
    sameRows: Int
  ): SparseGraph = {
    val n = g.n
    val invLab = new Array[Int](n)
    var i = 0
    while (i < n) {
      invLab(lab(i)) = i
      i += 1
    }

    // Compute total edges
    var totalEdges = 0L
    i = 0
    while (i < n) {
      totalEdges += g.degree(lab(i))
      i += 1
    }

    val newV = new Array[Long](n)
    val newD = new Array[Int](n)
    val newE = new Array[Int](totalEdges.toInt)

    var pos = 0
    i = 0
    while (i < n) {
      newV(i) = pos
      val dli = g.degree(lab(i))
      newD(i) = dli
      val neighbors = g.neighbors(lab(i))
      var j = 0
      while (j < dli) {
        newE(pos) = invLab(neighbors(j))
        pos += 1
        j += 1
      }
      i += 1
    }

    SparseGraph.fromData(newV, newD, newE, n, g.directed)
  }

  private def mash(l: Long, i: Int): Long = {
    ((l ^ 0x6B5D) + i) & 0x7FFF
  }
}

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
      return cleanup(longcode)
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

    // BFS optimization for early levels with singleton active cell
    // Matches C nauty nausparse.c lines 711-862
    if (level <= 2 && nactive == 1 && ptn(workActive(0)) <= level && numCells.value <= n / 8) {
      val isplit = workActive(0)
      nactive = 0
      SetOps.delElement(active, isplit)

      val distances = distvals(g, lab(isplit), n)

      // Refine non-singleton cells based on distance values
      var v1 = 0
      while (v1 < n) {
        if (ptn(v1) <= level) {
          v1 += 1
        } else {
          longcode = mash(longcode, v1)
          var w1 = distances(lab(v1))

          var v2 = v1 + 1
          while (ptn(v2 - 1) > level && distances(lab(v2)) == w1) v2 += 1

          if (ptn(v2 - 1) <= level) {
            // All vertices have same distance - no split
            v1 = v2
          } else {
            // Cell needs splitting - use do-while loop like C code
            var w2 = Int.MaxValue
            var v3 = v2
            var j = v2

            // C code: do { ... } while (ptn[j++] > level);
            var continue_loop = true
            while (continue_loop) {
              val lj = lab(j)
              val w3 = distances(lj)
              if (w3 == w1) {
                lab(j) = lab(v3)
                lab(v3) = lab(v2)
                lab(v2) = lj
                v2 += 1
                v3 += 1
              } else if (w3 == w2) {
                lab(j) = lab(v3)
                lab(v3) = lj
                v3 += 1
              } else if (w3 < w1) {
                lab(j) = lab(v2)
                lab(v2) = lab(v1)
                lab(v1) = lj
                v3 = v2 + 1
                v2 = v1 + 1
                w2 = w1
                w1 = w3
              } else if (w3 < w2) {
                lab(j) = lab(v2)
                lab(v2) = lj
                v3 = v2 + 1
                w2 = w3
              }
              // Check condition before incrementing j (matching C's ptn[j++])
              continue_loop = ptn(j) > level
              j += 1
            }

            longcode = mash(longcode, w2)
            longcode = mash(longcode, v2)

            if (j != v2) {
              // At least two fragments: v1..v2-1 = w1; v2..v3-1 = w2
              if (v2 == v1 + 1) cellStart(lab(v1)) = n
              if (v3 == v2 + 1) cellStart(lab(v2)) = n
              else {
                var k = v2
                while (k < v3) {
                  cellStart(lab(k)) = v2
                  k += 1
                }
              }
              numCells.value += 1
              ptn(v2 - 1) = level

              if (j == v3) {
                // Two fragments only - add smaller to active
                if (v2 - v1 <= v3 - v2 && !SetOps.isElement(active, v1)) {
                  SetOps.addElement(active, v1)
                  workActive(nactive) = v1
                  nactive += 1
                } else {
                  SetOps.addElement(active, v2)
                  workActive(nactive) = v2
                  nactive += 1
                }
              } else {
                // Extra fragments: v3..j-1 > w2, need to sort by distance
                // Sort lab[v3..j) by distances using indirect sort like C's sortindirect
                val extra = j - v3
                if (extra > 0) {
                  val indices = (v3 until j).sortBy(idx => distances(lab(idx))).toArray
                  val temp = new Array[Int](extra)
                  var k = 0
                  while (k < extra) {
                    temp(k) = lab(indices(k))
                    k += 1
                  }
                  System.arraycopy(temp, 0, lab, v3, extra)
                }

                SetOps.addElement(active, v2)
                workActive(nactive) = v2
                nactive += 1

                var bigpos = if (v2 - v1 >= v3 - v2) -1 else nactive - 1
                var bigsize = if (v2 - v1 >= v3 - v2) v2 - v1 else v3 - v2

                // Process extra fragments
                var k = v3 - 1
                while (k < j - 1) {
                  ptn(k) = level
                  longcode = mash(longcode, k)
                  numCells.value += 1
                  val l = k + 1
                  SetOps.addElement(active, l)
                  workActive(nactive) = l
                  nactive += 1
                  val w3local = distances(lab(l))
                  k = l
                  while (k < j - 1 && distances(lab(k + 1)) == w3local) {
                    cellStart(lab(k + 1)) = l
                    k += 1
                  }
                  val size = k - l + 1
                  if (size == 1) cellStart(lab(l)) = n
                  else {
                    cellStart(lab(l)) = l
                    if (size > bigsize) {
                      bigsize = size
                      bigpos = nactive - 1
                    }
                  }
                }

                if (bigpos >= 0 && !SetOps.isElement(active, v1)) {
                  longcode = mash(longcode, bigpos)
                  SetOps.delElement(active, workActive(bigpos))
                  SetOps.addElement(active, v1)
                  workActive(bigpos) = v1
                }
              }
            }
            v1 = j
          }
        }
      }
      return cleanup(longcode)
    }

    // Process active cells with singleton preference (C nauty lines 865-880)
    // Search first 10 cells for singletons, otherwise take from end (LIFO)
    while (nactive > 0 && numCells.value < n) {
      // Search first 10 active cells for a singleton
      var foundIdx = -1
      i = 0
      while (i < nactive && i < 10 && foundIdx < 0) {
        if (ptn(workActive(i)) <= level) {
          foundIdx = i
        }
        i += 1
      }

      val splitCell = if (foundIdx >= 0) {
        // Singleton found: swap with last and take
        val singleton = workActive(foundIdx)
        nactive -= 1
        workActive(foundIdx) = workActive(nactive)
        singleton
      } else {
        // No singleton in first 10: take from end (LIFO)
        nactive -= 1
        workActive(nactive)
      }

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

    // Update ptn to mark cell boundaries and track fragments for active set
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

    // Collect all fragment boundaries and sizes
    val fragmentStarts = new Array[Int](range + 1)
    val fragmentSizes = new Array[Int](range + 1)
    var numFragments = 0

    i = 0
    while (i < range) {
      val start = bucket(i)
      val end = if (i + 1 < range) bucket(i + 1) else cellSize
      if (end > start) {
        fragmentStarts(numFragments) = cellStart + start
        fragmentSizes(numFragments) = end - start
        numFragments += 1
      }
      i += 1
    }

    // Mark cell boundaries in ptn
    i = 1
    while (i < numFragments) {
      val boundaryPos = fragmentStarts(i) - 1
      ptn(boundaryPos) = level
      newCells += 1
      i += 1
    }

    // Handle active set updates based on number of fragments
    // Matches C nauty nausparse.c lines 1056-1119
    if (numFragments == 2) {
      // Two-way split: add smaller fragment to active (or v2 if equal and v1 in active)
      val v1 = fragmentStarts(0)
      val v2 = fragmentStarts(1)
      val size1 = fragmentSizes(0)
      val size2 = fragmentSizes(1)

      if (size1 <= size2 && !SetOps.isElement(active, v1)) {
        SetOps.addElement(active, v1)
        workActive(newActive) = v1
        newActive += 1
      } else {
        SetOps.addElement(active, v2)
        workActive(newActive) = v2
        newActive += 1
      }
    } else if (numFragments > 2) {
      // Multi-way split: add ALL fragments to active, then remove largest and swap with v1 if needed
      var bigIdx = -1
      var bigSize = fragmentSizes(0)  // v1 size is the baseline

      // Add all fragments except v1 to active
      i = 1
      while (i < numFragments) {
        val fragStart = fragmentStarts(i)
        val fragSize = fragmentSizes(i)
        SetOps.addElement(active, fragStart)
        workActive(newActive) = fragStart

        // Track largest fragment (only among newly added ones)
        if (fragSize > bigSize) {
          bigSize = fragSize
          bigIdx = newActive
        }
        newActive += 1
        i += 1
      }

      // If there's a largest fragment bigger than v1, and v1 not in active,
      // remove largest and add v1 instead
      val v1 = fragmentStarts(0)
      if (bigIdx >= 0 && !SetOps.isElement(active, v1)) {
        val bigStart = workActive(bigIdx)
        SetOps.delElement(active, bigStart)
        SetOps.addElement(active, v1)
        workActive(bigIdx) = v1
      }
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

  /**
   * Hash function matching C nauty's MASH macro.
   * The constant 0x6B5D has been verified to produce correct results.
   */
  @inline private def mash(l: Long, i: Int): Long = {
    ((l ^ 0x6B5D) + i) & 0x7FFF
  }

  /** Cleanup hash to 15 bits, matching C nauty's CLEANUP macro */
  @inline private def cleanup(l: Long): Int = (l % 0x7FFF).toInt

  /**
   * Compute BFS distances from source vertex to all vertices.
   * Matches C nauty's distvals() in nausparse.c lines 593-628.
   */
  private def distvals(g: SparseGraph, v0: Int, n: Int): Array[Int] = {
    val dist = Array.fill(n)(n)  // Initialize to n (infinity/unreachable)
    val queue = new Array[Int](n)
    var head = 0
    var tail = 1

    queue(0) = v0
    dist(v0) = 0

    while (head < tail && tail < n) {
      val i = queue(head)
      head += 1
      val neighbors = g.neighbors(i)
      var j = 0
      while (j < neighbors.length) {
        val k = neighbors(j)
        if (dist(k) == n) {
          dist(k) = dist(i) + 1
          queue(tail) = k
          tail += 1
        }
        j += 1
      }
    }
    dist
  }
}

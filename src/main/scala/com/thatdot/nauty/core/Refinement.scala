package com.thatdot.nauty.core

import com.thatdot.nauty.bits.{BitOps, SetOps}
import com.thatdot.nauty.graph.DenseGraph

/**
 * Partition refinement for graph automorphism computation.
 *
 * The refinement process makes a partition equitable by repeatedly splitting
 * cells based on adjacency counts to other cells.
 */
object Refinement {
  /** Hash function: depends on l and i, non-commutative */
  // Note: 0x6B1D = 065435 octal = 27421 decimal (matches C nauty's MASH macro)
  @inline private def mash(l: Long, i: Int): Long = {
    (((l ^ 0x6B1DL) + i) & 0x7FFFL)
  }

  /** Cleanup hash to fit in 15 bits */
  @inline private def cleanup(l: Long): Int = {
    (l % 0x7FFFL).toInt
  }

  /**
   * Refine a partition to make it equitable.
   *
   * @param g The graph
   * @param lab Current vertex labeling
   * @param ptn Partition array (ptn[i] > level means i and i+1 in same cell)
   * @param level Current level in the search tree
   * @param numCells Current number of cells (updated in place)
   * @param active Set of active cell start positions (modified during refinement)
   * @param m Number of setwords per row
   * @param n Number of vertices
   * @return A code that depends on the refinement details but not on vertex labeling
   */
  def refine(
    g: DenseGraph,
    lab: Array[Int],
    ptn: Array[Int],
    level: Int,
    numCells: IntRef,
    active: Array[Long],
    m: Int,
    n: Int
  ): Int = {
    val count = new Array[Int](n)
    val bucket = new Array[Int](n + 2)
    val workperm = new Array[Int](n)
    val workset = new Array[Long](m)

    var longcode: Long = numCells.value
    var split1 = -1
    var hint = 0
    var maxpos = 0

    while (numCells.value < n) {
      // Find next active cell to split with
      split1 = hint
      if (!SetOps.isElement(active, split1)) {
        split1 = SetOps.nextElement(active, m, split1)
        if (split1 < 0) {
          split1 = SetOps.nextElement(active, m, -1)
        }
      }

      if (split1 < 0) {
        // No more active cells
        longcode = mash(longcode, numCells.value)
        return cleanup(longcode)
      }

      SetOps.delElement(active, split1)

      // Find end of splitting cell
      var split2 = split1
      while (ptn(split2) > level) {
        split2 += 1
      }

      longcode = mash(longcode, split1 + split2)

      if (split1 == split2) {
        // Trivial splitting cell (singleton)
        val result = refineTrivialCellSinglePass(g, lab, ptn, level, numCells, active, split1, n, m, longcode)
        longcode = result._1
        hint = result._2
      } else {
        // Non-trivial splitting cell
        val result = refineNonTrivialCell(
          g, lab, ptn, level, numCells, active, count, bucket, workperm, workset,
          split1, split2, n, m, longcode
        )
        longcode = result._1
        hint = result._2
        maxpos = result._3
      }
    }

    longcode = mash(longcode, numCells.value)
    cleanup(longcode)
  }

  /**
   * Refine using a trivial (singleton) splitting cell.
   * Does everything in a single pass to ensure hint and longcode are computed
   * based on the original cell boundaries, before modifications.
   * Returns (longcode, hint).
   */
  private def refineTrivialCellSinglePass(
    g: DenseGraph,
    lab: Array[Int],
    ptn: Array[Int],
    level: Int,
    numCells: IntRef,
    active: Array[Long],
    split1: Int,
    n: Int,
    m: Int,
    initialCode: Long
  ): (Long, Int) = {
    var longcode = initialCode
    var hint = 0
    val gptr = g.graphRow(lab(split1))

    var cell1 = 0
    while (cell1 < n) {
      var cell2 = cell1
      while (ptn(cell2) > level) {
        cell2 += 1
      }

      if (cell1 != cell2) {
        // Non-singleton cell - try to split
        var c1 = cell1
        var c2 = cell2

        // Partition cell into adjacent/non-adjacent to lab[split1]
        while (c1 <= c2) {
          val labc1 = lab(c1)
          if (SetOps.isElement(gptr, labc1)) {
            c1 += 1
          } else {
            lab(c1) = lab(c2)
            lab(c2) = labc1
            c2 -= 1
          }
        }

        // If split occurred
        if (c2 >= cell1 && c1 <= cell2) {
          ptn(c2) = level
          longcode = mash(longcode, c2)
          numCells.value += 1

          // Add the smaller fragment to active set and compute hint
          if (SetOps.isElement(active, cell1) || c2 - cell1 >= cell2 - c1) {
            SetOps.addElement(active, c1)
            if (c1 == cell2) hint = c1
          } else {
            SetOps.addElement(active, cell1)
            if (c2 == cell1) hint = cell1
          }
        }
      }

      cell1 = cell2 + 1
    }
    (longcode, hint)
  }

  /**
   * Refine using a non-trivial splitting cell.
   * Returns (longcode, hint, maxpos).
   */
  private def refineNonTrivialCell(
    g: DenseGraph,
    lab: Array[Int],
    ptn: Array[Int],
    level: Int,
    numCells: IntRef,
    active: Array[Long],
    count: Array[Int],
    bucket: Array[Int],
    workperm: Array[Int],
    workset: Array[Long],
    split1: Int,
    split2: Int,
    n: Int,
    m: Int,
    initialCode: Long
  ): (Long, Int, Int) = {
    var longcode = initialCode
    var hint = 0
    var maxpos = 0

    // Build workset = vertices in splitting cell
    SetOps.emptySet(workset, m)
    var i = split1
    while (i <= split2) {
      SetOps.addElement(workset, lab(i))
      i += 1
    }
    longcode = mash(longcode, split2 - split1 + 1)

    // Process each cell
    var cell1 = 0
    while (cell1 < n) {
      var cell2 = cell1
      while (ptn(cell2) > level) {
        cell2 += 1
      }

      if (cell1 != cell2) {
        // Non-singleton cell - count adjacencies
        i = cell1
        var cnt = countIntersection(workset, g.graphRow(lab(i)), m)
        count(i) = cnt
        var bmin = cnt
        var bmax = cnt
        bucket(cnt) = 1

        i += 1
        while (i <= cell2) {
          cnt = countIntersection(workset, g.graphRow(lab(i)), m)
          while (bmin > cnt) {
            bmin -= 1
            bucket(bmin) = 0
          }
          while (bmax < cnt) {
            bmax += 1
            bucket(bmax) = 0
          }
          bucket(cnt) += 1
          count(i) = cnt
          i += 1
        }

        if (bmin == bmax) {
          // All vertices have same count - no split
          longcode = mash(longcode, bmin + cell1)
        } else {
          // Split based on counts
          var c1 = cell1
          var maxcell = -1

          i = bmin
          while (i <= bmax) {
            if (bucket(i) > 0) {
              val c2 = c1 + bucket(i)
              bucket(i) = c1
              longcode = mash(longcode, i + c1)

              if (c2 - c1 > maxcell) {
                maxcell = c2 - c1
                maxpos = c1
              }

              if (c1 != cell1) {
                SetOps.addElement(active, c1)
                if (c2 - c1 == 1) hint = c1
                numCells.value += 1
              }
              if (c2 <= cell2) ptn(c2 - 1) = level
              c1 = c2
            }
            i += 1
          }

          // Permute lab according to bucket sort
          i = cell1
          while (i <= cell2) {
            workperm(bucket(count(i))) = lab(i)
            bucket(count(i)) += 1
            i += 1
          }
          i = cell1
          while (i <= cell2) {
            lab(i) = workperm(i)
            i += 1
          }

          // Ensure cell1 is in active set
          if (!SetOps.isElement(active, cell1)) {
            SetOps.addElement(active, cell1)
            SetOps.delElement(active, maxpos)
          }
        }
      }

      cell1 = cell2 + 1
    }

    (longcode, hint, maxpos)
  }

  /**
   * Count the size of intersection of two sets.
   */
  @inline private def countIntersection(set1: Array[Long], set2: Array[Long], m: Int): Int = {
    var cnt = 0
    var i = 0
    while (i < m) {
      val x = set1(i) & set2(i)
      if (x != 0L) cnt += BitOps.popcount(x)
      i += 1
    }
    cnt
  }
}

/**
 * Mutable reference to an Int, used to pass numCells by reference.
 */
final class IntRef(var value: Int)

object IntRef {
  def apply(v: Int): IntRef = new IntRef(v)
}

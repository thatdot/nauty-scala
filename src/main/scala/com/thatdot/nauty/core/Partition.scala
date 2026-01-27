package com.thatdot.nauty.core

/**
 * A partition of {0, 1, ..., n-1} into ordered cells.
 *
 * Uses nauty's encoding:
 * - lab[0..n-1] contains the vertices in order, grouped by cells
 * - ptn[i] > level means vertex i is in the same cell as vertex i+1
 * - ptn[i] <= level means vertex i is the last in its cell
 *
 * At level 0, ptn[i] = 0 marks cell boundaries.
 * At level k, cells are defined by ptn[i] <= k.
 *
 * This class is mutable for efficiency during refinement.
 *
 * @param lab Vertex labeling array
 * @param ptn Partition indicator array
 * @param n   Number of vertices
 */
final class Partition private[core](
  val lab: Array[Int],
  val ptn: Array[Int],
  val n: Int
) {
  /**
   * Get the inverse labeling (invlab[lab[i]] = i).
   */
  def inverseLab: Array[Int] = {
    val invlab = new Array[Int](n)
    var i = 0
    while (i < n) {
      invlab(lab(i)) = i
      i += 1
    }
    invlab
  }

  /**
   * Count the number of cells at the given level.
   */
  def numCells(level: Int = 0): Int = {
    var count = 0
    var i = 0
    while (i < n) {
      if (ptn(i) <= level) count += 1
      i += 1
    }
    count
  }

  /**
   * Check if the partition is discrete (each vertex in its own cell).
   */
  def isDiscrete: Boolean = numCells(0) == n

  /**
   * Get the cell containing position i in lab.
   * Returns (start, end) indices in lab.
   */
  def cellAt(i: Int, level: Int = 0): (Int, Int) = {
    // Find start of cell
    var start = i
    while (start > 0 && ptn(start - 1) > level) {
      start -= 1
    }
    // Find end of cell
    var end = i
    while (end < n - 1 && ptn(end) > level) {
      end += 1
    }
    (start, end + 1)  // end is exclusive
  }

  /**
   * Get the vertices in the cell containing position i.
   */
  def cellVertices(i: Int, level: Int = 0): IndexedSeq[Int] = {
    val (start, end) = cellAt(i, level)
    lab.slice(start, end).toIndexedSeq
  }

  /**
   * Get all cells as sequences of vertices.
   */
  def cells(level: Int = 0): IndexedSeq[IndexedSeq[Int]] = {
    val result = IndexedSeq.newBuilder[IndexedSeq[Int]]
    var i = 0
    while (i < n) {
      val (start, end) = cellAt(i, level)
      result += lab.slice(start, end).toIndexedSeq
      i = end
    }
    result.result()
  }

  /**
   * Find the first non-singleton cell at the given level.
   * Returns the start index in lab, or -1 if all cells are singletons.
   */
  def firstNonSingletonCell(level: Int = 0): Int = {
    var i = 0
    while (i < n) {
      if (ptn(i) > level) {
        // This cell has more than one element
        return i
      }
      i += 1
    }
    -1
  }

  /**
   * Split a cell at position splitPoint.
   * Vertices lab[start..splitPoint-1] become one cell,
   * vertices lab[splitPoint..end-1] become another cell.
   *
   * @param cellStart Start of the cell to split
   * @param splitPoint Position where split occurs (must be > cellStart)
   * @param level Current level
   */
  def splitCell(cellStart: Int, splitPoint: Int, level: Int): Unit = {
    require(splitPoint > cellStart, "Split point must be after cell start")
    // Mark the boundary at splitPoint-1
    ptn(splitPoint - 1) = level
  }

  /**
   * Create a snapshot of this partition for backtracking.
   */
  def snapshot(): PartitionSnapshot = {
    new PartitionSnapshot(lab.clone(), ptn.clone(), n)
  }

  /**
   * Restore from a snapshot.
   */
  def restore(snap: PartitionSnapshot): Unit = {
    require(snap.n == n, "Snapshot has different size")
    System.arraycopy(snap.lab, 0, lab, 0, n)
    System.arraycopy(snap.ptn, 0, ptn, 0, n)
  }

  /**
   * Copy the partition to new arrays.
   */
  def copy: Partition = {
    new Partition(lab.clone(), ptn.clone(), n)
  }

  /**
   * Format the partition for display.
   */
  override def toString: String = {
    cells().map(c => "[" + c.mkString(" ") + "]").mkString(" | ")
  }

  /**
   * Detailed debug string showing lab and ptn arrays.
   */
  def toDebugString: String = {
    s"Partition(n=$n)\n  lab: ${lab.mkString("[", ", ", "]")}\n  ptn: ${ptn.mkString("[", ", ", "]")}"
  }
}

object Partition {
  /**
   * Create a trivial partition (all vertices in one cell).
   */
  def trivial(n: Int): Partition = {
    val lab = new Array[Int](n)
    val ptn = new Array[Int](n)
    var i = 0
    while (i < n) {
      lab(i) = i
      ptn(i) = 1  // All connected except last
      i += 1
    }
    if (n > 0) ptn(n - 1) = 0  // Last vertex ends the cell
    new Partition(lab, ptn, n)
  }

  /**
   * Create a discrete partition (each vertex in its own cell).
   */
  def discrete(n: Int): Partition = {
    val lab = new Array[Int](n)
    val ptn = new Array[Int](n)
    var i = 0
    while (i < n) {
      lab(i) = i
      ptn(i) = 0  // Each vertex is a singleton cell
      i += 1
    }
    new Partition(lab, ptn, n)
  }

  /**
   * Create a partition from cells.
   */
  def fromCells(n: Int, cells: Seq[Seq[Int]]): Partition = {
    val lab = new Array[Int](n)
    val ptn = new Array[Int](n)

    var pos = 0
    for (cell <- cells) {
      for (v <- cell) {
        lab(pos) = v
        pos += 1
      }
      // Mark cell boundaries
      var i = pos - cell.length
      while (i < pos - 1) {
        ptn(i) = 1  // Not end of cell
        i += 1
      }
      ptn(pos - 1) = 0  // End of cell
    }

    require(pos == n, s"Cells don't cover all $n vertices (got $pos)")
    new Partition(lab, ptn, n)
  }

  /**
   * Create from existing lab/ptn arrays (arrays are copied).
   */
  def fromArrays(lab: Array[Int], ptn: Array[Int]): Partition = {
    require(lab.length == ptn.length, "lab and ptn must have same length")
    new Partition(lab.clone(), ptn.clone(), lab.length)
  }

  /**
   * Create using existing arrays without copying (for internal use).
   */
  private[nauty] def unsafeFromArrays(lab: Array[Int], ptn: Array[Int], n: Int): Partition = {
    new Partition(lab, ptn, n)
  }
}

/**
 * An immutable snapshot of a partition state.
 */
final class PartitionSnapshot private[core](
  private[core] val lab: Array[Int],
  private[core] val ptn: Array[Int],
  val n: Int
) {
  def toPartition: Partition = {
    new Partition(lab.clone(), ptn.clone(), n)
  }
}

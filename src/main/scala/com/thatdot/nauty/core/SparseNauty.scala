package com.thatdot.nauty.core

import com.thatdot.nauty.bits.{SetWord, SetOps}
import com.thatdot.nauty.graph.SparseGraph
import com.thatdot.nauty.group.{Permutation, Orbits, SchreierSims}
import com.thatdot.nauty.util.{NautyOptions, NautyStats, StatsBuilder}

import scala.collection.mutable.ArrayBuffer

/**
 * Result of a sparse nauty computation.
 */
case class SparseNautyResult(
  canonicalGraph: Option[SparseGraph],
  canonicalLabeling: Array[Int],
  generators: List[Permutation],
  groupSize: BigDecimal,
  numOrbits: Int,
  orbits: Array[Int],
  stats: NautyStats
)

/**
 * Nauty algorithm implementation for sparse graphs.
 *
 * This operates directly on sparse graph representation without
 * converting to dense, which is more efficient for large sparse graphs.
 */
object SparseNauty {
  /**
   * Compute automorphism group and canonical labeling of a sparse graph.
   *
   * @param g       The input sparse graph
   * @param options Computation options
   * @return SparseNautyResult with generators, canonical form, etc.
   */
  def sparsenauty(g: SparseGraph, options: NautyOptions = NautyOptions.defaultSparseGraph): SparseNautyResult = {
    val n = g.n

    // Handle empty graph
    if (n == 0) {
      return SparseNautyResult(
        canonicalGraph = if (options.getCanon != 0) Some(SparseGraph.empty(0)) else None,
        canonicalLabeling = Array.empty[Int],
        generators = Nil,
        groupSize = BigDecimal(1),
        numOrbits = 0,
        orbits = Array.empty[Int],
        stats = NautyStats.empty
      )
    }

    val context = new SparseNautyContext(g, options, n)
    context.run()
  }

  /**
   * Test if two sparse graphs are isomorphic.
   */
  def isIsomorphic(g1: SparseGraph, g2: SparseGraph): Boolean = {
    if (g1.n != g2.n || g1.numEdges != g2.numEdges) return false

    val opts = NautyOptions.defaultSparseGraph.withCanon
    val r1 = sparsenauty(g1, opts)
    val r2 = sparsenauty(g2, opts)

    (r1.canonicalGraph, r2.canonicalGraph) match {
      case (Some(c1), Some(c2)) => c1 == c2
      case _ => false
    }
  }

  /**
   * Compute the canonical form of a sparse graph.
   */
  def canonicalForm(g: SparseGraph): SparseGraph = {
    val result = sparsenauty(g, NautyOptions.defaultSparseGraph.withCanon)
    result.canonicalGraph.get
  }
}

/**
 * Internal context for a sparse nauty computation.
 */
private class SparseNautyContext(
  val g: SparseGraph,
  val options: NautyOptions,
  val n: Int
) {
  private val NAUTY_INFINITY = 2000000002
  private val m = SetWord.setwordsNeeded(n)

  // Working arrays
  val lab: Array[Int] = new Array[Int](n)
  val ptn: Array[Int] = new Array[Int](n)
  val orbits: Array[Int] = new Array[Int](n)
  val active: Array[Long] = new Array[Long](m)

  // First path data
  val firstLab: Array[Int] = new Array[Int](n)
  val firstCode: Array[Int] = new Array[Int](n + 2)
  val firstTc: Array[Int] = new Array[Int](n + 2)

  // Canonical data
  val canonLab: Array[Int] = new Array[Int](n)
  var canong: Option[SparseGraph] = None

  // Search state
  var gcaFirst: Int = 0
  var gcaCanon: Int = 0
  var eqlevFirst: Int = 0
  var eqlevCanon: Int = -1
  var compCanon: Int = 0
  var canonLevel: Int = 0
  var allSameLevel: Int = 0  // Level of least ancestor where all descendants are equivalent
  var nonCheapLevel: Int = 1 // Level of greatest ancestor where cheapautom==FALSE
  var stabVertex: Int = 0    // Vertex fixed in ancestor of first leaf at level gcaCanon
  var cosetIndex: Int = 0    // The vertex being fixed at level gcaFirst

  // Statistics
  val stats = new StatsBuilder()
  val generators = new ArrayBuffer[Permutation]()

  /**
   * Initialize the partition.
   */
  def initPartition(): Int = {
    if (options.defaultPtn) {
      var i = 0
      while (i < n) {
        lab(i) = i
        ptn(i) = NAUTY_INFINITY
        i += 1
      }
      ptn(n - 1) = 0
      SetOps.emptySet(active, m)
      SetOps.addElement(active, 0)
      1
    } else {
      options.initialPartition match {
        case Some(cells) =>
          var pos = 0
          var numCells = 0
          SetOps.emptySet(active, m)

          for (cell <- cells) {
            val cellStart = pos
            for (v <- cell) {
              lab(pos) = v
              ptn(pos) = NAUTY_INFINITY
              pos += 1
            }
            if (pos > 0) {
              ptn(pos - 1) = 0
            }
            SetOps.addElement(active, cellStart)
            numCells += 1
          }

          require(pos == n, s"Initial partition must cover all $n vertices, got $pos")
          numCells

        case None =>
          throw new IllegalStateException("defaultPtn=false but no initialPartition provided")
      }
    }
  }

  /**
   * Initialize orbits.
   */
  def initOrbits(): Unit = {
    var i = 0
    while (i < n) {
      orbits(i) = i
      i += 1
    }
    stats.numOrbits = n
  }

  /**
   * Check if partition cells are likely to be orbits (cheap heuristic).
   * Returns TRUE if a simple sufficient condition is met for the partition
   * cells to be orbits of some subgroup.
   * Always returns FALSE for digraphs.
   */
  def cheapAutom(level: Int): Boolean = {
    if (g.isDigraph) return false

    var k = n
    var nnt = 0
    var i = 0
    while (i < n) {
      k -= 1
      if (ptn(i) > level) {
        nnt += 1
        while (i < n - 1 && ptn(i) > level) {
          i += 1
        }
      }
      i += 1
    }

    k <= nnt + 1 || k <= 4
  }

  /**
   * Run the sparse nauty algorithm.
   */
  def run(): SparseNautyResult = {
    val numCells = initPartition()
    initOrbits()

    // Perform initial refinement using sparse refinement
    val numCellsRef = IntRef(numCells)
    SparseRefinement.refine(g, lab, ptn, 0, numCellsRef, active, m, n)

    // Run the search
    firstPathNode(1, numCellsRef.value)

    // Build result
    stats.numOrbits = Orbits.numOrbits(orbits, n)

    val generatorsList = generators.map(p => Permutation.unsafeFromArray(p.toArray)).toList

    // Compute group size
    val groupSize: BigDecimal = if (options.schreier && generatorsList.nonEmpty) {
      BigDecimal(SchreierSims.groupOrder(generatorsList, n))
    } else {
      stats.build().groupSize
    }

    SparseNautyResult(
      canonicalGraph = if (options.getCanon != 0) canong else None,
      canonicalLabeling = if (options.getCanon != 0) canonLab.clone() else lab.clone(),
      generators = generatorsList,
      groupSize = groupSize,
      numOrbits = stats.numOrbits,
      orbits = orbits.clone(),
      stats = stats.build()
    )
  }

  /**
   * Process first path in search tree.
   */
  def firstPathNode(level: Int, numCells: Int): Int = {
    stats.numNodes += 1

    if (numCells == n) {
      firstTerminal(level)
      return level - 1
    }

    val tcellSize = makeTargetCell(level, numCells)
    stats.tcTotal += tcellSize
    firstTc(level) = findCellStart(level)

    saveState(level)

    val tv1 = lab(firstTc(level))

    // Check cheapautom and update nonCheapLevel
    if (nonCheapLevel >= level && !cheapAutom(level)) {
      nonCheapLevel = level + 1
    }

    cosetIndex = tv1
    breakout(level, firstTc(level), tv1)

    val numCellsRef = IntRef(numCells + 1)
    val code = SparseRefinement.refine(g, lab, ptn, level, numCellsRef, active, m, n)
    firstCode(level) = code

    var retval = firstPathNode(level + 1, numCellsRef.value)
    gcaFirst = level
    stabVertex = tv1

    if (retval >= level) {
      restoreState(level)
      retval = otherNode(level, numCells)
    }

    // Group size calculation
    val (_, tcEnd) = cellBounds(firstTc(level), level - 1)
    var index = 0
    var i = firstTc(level)
    while (i < tcEnd) {
      val v = labStack(level)(i)
      if (Orbits.sameOrbit(orbits, v, tv1)) {
        index += 1
      }
      i += 1
    }
    if (index > 0) {
      stats.multiplyGroupSize(index)
    }

    // Update allSameLevel
    if (tcellSize == index && allSameLevel == level + 1) {
      allSameLevel -= 1
    }

    retval
  }

  /**
   * Process non-first path nodes.
   */
  def otherNode(level: Int, numCells: Int): Int = {
    val tcStart = firstTc(level)
    val (_, tcEnd) = cellBounds(tcStart, level - 1)

    // Check cheapautom and update nonCheapLevel
    if (!cheapAutom(level)) {
      nonCheapLevel = level + 1
    }

    var i = tcStart + 1
    while (i < tcEnd) {
      restoreState(level)
      stats.numNodes += 1

      eqlevFirst = level - 1

      val tv = lab(i)
      if (orbits(tv) == tv) {
        breakout(level, tcStart, tv)

        val numCellsRef = IntRef(numCells + 1)
        val code = SparseRefinement.refine(g, lab, ptn, level, numCellsRef, active, m, n)

        if (code < firstCode(level)) {
          stats.numBadLeaves += 1
        } else if (code > firstCode(level)) {
          stats.numBadLeaves += 1
        } else {
          eqlevFirst = level
          val retval = processNode(level + 1, numCellsRef.value)
          if (retval < level) return retval
        }
      }
      i += 1
    }

    level - 1
  }

  /**
   * Process a general search tree node.
   */
  def processNode(level: Int, numCells: Int): Int = {
    stats.numNodes += 1

    if (numCells == n) {
      return processLeaf(level)
    }

    val tcellSize = makeTargetCell(level, numCells)
    stats.tcTotal += tcellSize
    val tcStart = findCellStart(level)

    saveState(level)

    val (_, tcEnd) = cellBounds(tcStart, level - 1)
    var i = tcStart

    while (i < tcEnd) {
      if (i > tcStart) restoreState(level)

      val tv = lab(i)
      breakout(level, tcStart, tv)

      val numCellsRef = IntRef(numCells + 1)
      val code = SparseRefinement.refine(g, lab, ptn, level, numCellsRef, active, m, n)

      if (code == firstCode(level)) {
        if (eqlevFirst == level - 1) {
          eqlevFirst = level
        }
        val retval = processNode(level + 1, numCellsRef.value)
        if (retval < level - 1) return retval
      } else {
        if (options.getCanon != 0) {
          val retval = processNode(level + 1, numCellsRef.value)
          if (retval < level - 1) return retval
        }
      }

      i += 1
    }

    level - 1
  }

  /**
   * Process first leaf.
   */
  def firstTerminal(level: Int): Unit = {
    stats.maxLevel = math.max(stats.maxLevel, level)
    System.arraycopy(lab, 0, firstLab, 0, n)

    gcaFirst = level
    gcaCanon = level
    canonLevel = level
    eqlevCanon = level
    allSameLevel = level

    if (options.getCanon != 0) {
      System.arraycopy(lab, 0, canonLab, 0, n)
      updateCanon(level)
      stats.canUpdates += 1
    }
  }

  /**
   * Process a non-first leaf.
   * Returns the level to return to (for pruning).
   */
  def processLeaf(level: Int): Int = {
    stats.maxLevel = math.max(stats.maxLevel, level)

    // Determine which case we're in
    var code = 0

    // Check for automorphism equivalent to first leaf
    if (eqlevFirst >= level - 1 && isAutomorphism()) {
      code = 1
    } else if (options.getCanon != 0) {
      val cmp = compareWithCanon()
      if (cmp == 0) {
        code = 2
      } else if (cmp > 0) {
        code = 3
      } else {
        code = 4
      }
    } else {
      code = 4
    }

    code match {
      case 1 =>
        val perm = new Array[Int](n)
        computePermutation(perm)
        recordAutomorphism(perm)
        gcaFirst

      case 2 =>
        val perm = new Array[Int](n)
        var i = 0
        while (i < n) {
          perm(canonLab(i)) = lab(i)
          i += 1
        }
        val oldNumOrbits = stats.numOrbits
        val numOrbits = Orbits.orbjoin(orbits, perm, n)
        stats.numOrbits = numOrbits

        if (numOrbits == oldNumOrbits) {
          gcaCanon
        } else {
          generators += Permutation.fromArray(perm)
          stats.numGenerators += 1
          if (orbits(cosetIndex) < cosetIndex) {
            gcaFirst
          } else {
            gcaCanon
          }
        }

      case 3 =>
        System.arraycopy(lab, 0, canonLab, 0, n)
        updateCanon(level)
        stats.canUpdates += 1
        canonLevel = level
        eqlevCanon = level
        gcaCanon = level
        compCanon = 0
        computeNewLevel(level)

      case 4 =>
        stats.numBadLeaves += 1
        computeNewLevel(level)

      case _ =>
        level - 1
    }
  }

  /**
   * Compute the return level using allSameLevel and nonCheapLevel.
   */
  private def computeNewLevel(level: Int): Int = {
    // TODO: Re-enable allSameLevel optimization once tracking is verified correct
    // val save = if (allSameLevel > eqlevCanon) allSameLevel - 1 else eqlevCanon
    // if (nonCheapLevel <= save) nonCheapLevel - 1 else save
    level - 1  // Simple fallback: just return to parent level
  }

  /**
   * Check if current labeling gives an automorphism.
   * Uses sparse automorphism testing.
   */
  def isAutomorphism(): Boolean = {
    val perm = new Array[Int](n)
    computePermutation(perm)
    SparseRefinement.isAutomorphism(g, perm, options.digraph)
  }

  /**
   * Compute the permutation from firstLab to lab.
   */
  def computePermutation(perm: Array[Int]): Unit = {
    var i = 0
    while (i < n) {
      perm(firstLab(i)) = lab(i)
      i += 1
    }
  }

  /**
   * Record an automorphism.
   */
  def recordAutomorphism(perm: Array[Int]): Boolean = {
    val oldNumOrbits = stats.numOrbits
    val numOrbits = Orbits.orbjoin(orbits, perm, n)
    stats.numOrbits = numOrbits

    var numFixed = 0
    var i = 0
    while (i < n) {
      if (perm(i) == i) numFixed += 1
      i += 1
    }

    val isNewGenerator = numFixed < n && numOrbits < oldNumOrbits

    if (isNewGenerator) {
      generators += Permutation.fromArray(perm)
      stats.numGenerators += 1
    }

    options.userAutomProc.foreach { callback =>
      callback(stats.numGenerators, perm, orbits, numOrbits, numFixed, n)
    }

    isNewGenerator
  }

  /**
   * Compare current labeling with canonical.
   */
  def compareWithCanon(): Int = {
    canong match {
      case Some(cg) =>
        val (cmp, _) = SparseRefinement.testCanonLab(g, cg, lab)
        cmp
      case None =>
        -1
    }
  }

  /**
   * Update the canonical graph.
   */
  def updateCanon(level: Int): Unit = {
    canong = Some(SparseRefinement.updateCanon(g, lab, 0))
  }

  /**
   * Break out a vertex from a cell.
   */
  def breakout(level: Int, tcStart: Int, tv: Int): Unit = {
    SetOps.emptySet(active, m)
    SetOps.addElement(active, tcStart)

    var i = tcStart
    var prev = tv

    while (lab(i) != tv) {
      val next = lab(i)
      lab(i) = prev
      prev = next
      i += 1
    }
    lab(i) = prev

    ptn(tcStart) = level
  }

  /**
   * Make target cell for this level.
   */
  def makeTargetCell(level: Int, numCells: Int): Int = {
    var i = 0
    while (i < n && ptn(i) <= level) {
      i += 1
    }

    if (i == n) {
      0
    } else {
      val start = i
      while (i < n - 1 && ptn(i) > level) {
        i += 1
      }
      i - start + 1
    }
  }

  /**
   * Find the starting position of the target cell.
   */
  def findCellStart(level: Int): Int = {
    var i = 0
    while (i < n && ptn(i) <= level) {
      i += 1
    }
    if (i == n) 0 else i
  }

  /**
   * Get bounds of cell containing position i.
   */
  def cellBounds(i: Int, level: Int): (Int, Int) = {
    var start = i
    while (start > 0 && ptn(start - 1) > level) {
      start -= 1
    }
    var end = i
    while (end < n - 1 && ptn(end) > level) {
      end += 1
    }
    (start, end + 1)
  }

  // State saving for backtracking
  private val labStack: Array[Array[Int]] = Array.fill(n + 1)(new Array[Int](n))
  private val ptnStack: Array[Array[Int]] = Array.fill(n + 1)(new Array[Int](n))

  def saveState(level: Int): Unit = {
    System.arraycopy(lab, 0, labStack(level), 0, n)
    System.arraycopy(ptn, 0, ptnStack(level), 0, n)
  }

  def restoreState(level: Int): Unit = {
    System.arraycopy(labStack(level), 0, lab, 0, n)
    System.arraycopy(ptnStack(level), 0, ptn, 0, n)
  }
}

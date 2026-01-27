package com.thatdot.nauty.core

import com.thatdot.nauty.bits.{SetWord, BitOps, SetOps}
import com.thatdot.nauty.graph.{DenseGraph, SparseGraph, Graph}
import com.thatdot.nauty.group.{Permutation, Orbits, SchreierSims}
import com.thatdot.nauty.util.{NautyOptions, NautyStats, StatsBuilder}

import scala.collection.mutable.ArrayBuffer

/**
 * Result of a nauty computation.
 *
 * @param canonicalGraph     The canonically labeled graph (if requested)
 * @param canonicalLabeling  The permutation mapping input to canonical form
 * @param generators         Generators of the automorphism group
 * @param groupSize          Size of the automorphism group
 * @param numOrbits          Number of orbits
 * @param orbits             Orbit partition (orbits(i) = representative of i's orbit)
 * @param stats              Computation statistics
 */
case class NautyResult(
  canonicalGraph: Option[DenseGraph],
  canonicalLabeling: Array[Int],
  generators: List[Permutation],
  groupSize: BigDecimal,
  numOrbits: Int,
  orbits: Array[Int],
  stats: NautyStats
) {
  /**
   * Compute a canonical hash of the graph.
   * This produces a hex string that is the same for isomorphic graphs.
   */
  def canonicalHash: String = {
    canonicalGraph match {
      case Some(g) =>
        // Use same algorithm as nauty's hash: based on adjacency matrix data
        val data = g.data
        var h1 = 0L
        var h2 = 0L
        var h3 = 0L

        var i = 0
        while (i < data.length) {
          val w = data(i)
          h1 = h1 ^ (w * (i + 1))
          h2 = h2 ^ java.lang.Long.rotateLeft(w, (i * 17) % 64)
          h3 = h3 + (w ^ (h1 >> 32))
          i += 1
        }

        // Include n for disambiguating different sizes
        h1 = h1 ^ (g.n.toLong << 32)
        h3 = h3 ^ g.numEdges

        f"[${h1.toHexString}%s ${h2.toHexString}%s ${h3.toHexString}%s]"

      case None =>
        "[no-canon]"
    }
  }
}

/**
 * Main nauty algorithm implementation.
 *
 * This implements the fundamental graph canonization and automorphism group
 * computation algorithm of Brendan McKay.
 */
object Nauty {
  private val NAUTY_INFINITY = 2000000002

  /**
   * Compute automorphism group and canonical labeling of a dense graph.
   *
   * @param g       The input graph
   * @param options Computation options
   * @return NautyResult with generators, canonical form, etc.
   */
  def densenauty(g: DenseGraph, options: NautyOptions = NautyOptions.defaultGraph): NautyResult = {
    val n = g.n
    val m = g.m

    // Handle empty graph
    if (n == 0) {
      return NautyResult(
        canonicalGraph = if (options.getCanon != 0) Some(DenseGraph.empty(0)) else None,
        canonicalLabeling = Array.empty[Int],
        generators = Nil,
        groupSize = BigDecimal(1),
        numOrbits = 0,
        orbits = Array.empty[Int],
        stats = NautyStats.empty
      )
    }

    val context = new NautyContext(g, options, n, m)
    context.run()
  }

  /**
   * Compute automorphism group and canonical labeling of a sparse graph.
   * This uses native sparse-specific algorithms without converting to dense.
   *
   * @param g       The input sparse graph
   * @param options Computation options
   * @return SparseNautyResult with sparse canonical graph (no conversion to dense)
   */
  def sparsenauty(g: SparseGraph, options: NautyOptions = NautyOptions.defaultSparseGraph): SparseNautyResult = {
    SparseNauty.sparsenauty(g, options)
  }

  /**
   * Test if two graphs are isomorphic.
   */
  def isIsomorphic(g1: DenseGraph, g2: DenseGraph): Boolean = {
    if (g1.n != g2.n || g1.numEdges != g2.numEdges) return false

    val opts = NautyOptions.defaultGraph.withCanon
    val r1 = densenauty(g1, opts)
    val r2 = densenauty(g2, opts)

    (r1.canonicalGraph, r2.canonicalGraph) match {
      case (Some(c1), Some(c2)) => c1 == c2
      case _ => false
    }
  }

  /**
   * Compute the canonical form of a graph.
   */
  def canonicalForm(g: DenseGraph): DenseGraph = {
    val result = densenauty(g, NautyOptions.defaultGraph.withCanon)
    result.canonicalGraph.get
  }

  /**
   * Compute canonical hash with initial partition (vertex coloring).
   *
   * @param g          The graph
   * @param partition  Vertex partition - vertices in same cell are considered equivalent
   * @param directed   Whether graph is directed
   * @return Canonical hash string
   */
  def canonicalHash(g: DenseGraph, partition: Seq[Seq[Int]], directed: Boolean = false): String = {
    val opts = NautyOptions(
      getCanon = 1,
      digraph = directed,
      defaultPtn = false,
      initialPartition = Some(partition)
    )
    val result = densenauty(g, opts)
    result.canonicalHash
  }

  /**
   * Get automorphism generators with initial partition.
   *
   * @param g          The graph
   * @param partition  Vertex partition
   * @param directed   Whether graph is directed
   * @return List of generators as permutations
   */
  def automorphismGenerators(g: DenseGraph, partition: Seq[Seq[Int]], directed: Boolean = false): List[Permutation] = {
    val opts = NautyOptions(
      getCanon = 0,
      digraph = directed,
      defaultPtn = false,
      initialPartition = Some(partition)
    )
    val result = densenauty(g, opts)
    result.generators
  }

  /**
   * Generate all elements of the automorphism group from generators.
   * Warning: This can be exponential in the group size!
   *
   * @param generators List of generators
   * @param maxSize    Maximum number of elements to generate (safety limit)
   * @return Set of all group elements as permutations
   */
  def generateGroup(generators: List[Permutation], maxSize: Int = 10000): Set[Permutation] = {
    if (generators.isEmpty) {
      Set.empty
    } else {
      val n = generators.head.size
      val identity = Permutation.identity(n)
      var group = Set(identity)
      var frontier = generators.toSet

      while (frontier.nonEmpty && group.size < maxSize) {
        val next = scala.collection.mutable.Set[Permutation]()
        for {
          g <- group
          h <- generators
        } {
          val gh = g.compose(h)
          if (!group.contains(gh)) {
            next += gh
          }
          val hg = h.compose(g)
          if (!group.contains(hg)) {
            next += hg
          }
        }
        if (next.isEmpty) {
          // Closure reached
          frontier = Set.empty
        } else {
          group = group ++ next
          frontier = next.toSet
        }
      }
      group
    }
  }
}

/**
 * Internal context for a nauty computation.
 * Contains all mutable state for the search.
 */
private class NautyContext(
  val g: DenseGraph,
  val options: NautyOptions,
  val n: Int,
  val m: Int
) {
  private val NAUTY_INFINITY = 2000000002

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
  val canonCode: Array[Int] = new Array[Int](n + 2)
  var canong: Option[DenseGraph] = None

  // Search state
  var gcaFirst: Int = 0      // GCA with first leaf
  var gcaCanon: Int = 0      // GCA with best leaf
  var eqlevFirst: Int = 0    // Level to which codes match first
  var eqlevCanon: Int = -1   // Level to which codes match canon
  var compCanon: Int = 0     // Comparison with canon
  var canonLevel: Int = 0    // Level of best leaf
  var sameRows: Int = 0      // Rows matching in canong
  var allSameLevel: Int = 0
  var nonCheapLevel: Int = 1

  // Statistics
  val stats = new StatsBuilder()
  val generators = new ArrayBuffer[Permutation]()

  // Work arrays for refinement
  val count: Array[Int] = new Array[Int](n)
  val bucket: Array[Int] = new Array[Int](n + 2)
  val workperm: Array[Int] = new Array[Int](n)
  val workset: Array[Long] = new Array[Long](m)
  val fixedpts: Array[Long] = new Array[Long](m)
  val tcell: Array[Long] = new Array[Long](m)

  // Target cell stack for backtracking
  val tcellStack: Array[Array[Long]] = Array.fill(n + 1)(new Array[Long](m))

  /**
   * Initialize the partition.
   */
  def initPartition(): Int = {
    if (options.defaultPtn) {
      // All vertices in one cell
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
      // User-defined partition
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
            // Mark end of cell
            if (pos > 0) {
              ptn(pos - 1) = 0
            }
            // Add to active set
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
   * Run the nauty algorithm.
   */
  def run(): NautyResult = {
    val numCells = initPartition()
    initOrbits()
    SetOps.emptySet(fixedpts, m)

    // Perform initial refinement
    val numCellsRef = IntRef(numCells)
    val code = Refinement.refine(g, lab, ptn, 0, numCellsRef, active, m, n)

    // Run the search
    firstPathNode(1, numCellsRef.value)

    // Build result
    stats.numOrbits = Orbits.numOrbits(orbits, n)

    val generatorsList = generators.map(p => Permutation.unsafeFromArray(p.toArray)).toList

    // Compute group size: use Schreier-Sims if enabled, otherwise use orbit-index approximation
    val groupSize: BigDecimal = if (options.schreier && generatorsList.nonEmpty) {
      // Use Schreier-Sims for accurate group order
      BigDecimal(SchreierSims.groupOrder(generatorsList, n))
    } else {
      // Use the orbit-index approximation computed during search
      stats.build().groupSize
    }

    NautyResult(
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
      // Reached a leaf
      firstTerminal(level)
      return level - 1
    }

    // Find target cell
    val tcellSize = makeTargetCell(level, numCells)
    stats.tcTotal += tcellSize
    firstTc(level) = findCellStart(level)

    // Save state
    saveState(level)

    // Remember the first vertex in target cell (tv1 in C nauty)
    val tv1 = lab(firstTc(level))

    // Process first element
    breakout(level, firstTc(level), tv1)

    // Refine
    val numCellsRef = IntRef(numCells + 1)
    val code = Refinement.refine(g, lab, ptn, level, numCellsRef, active, m, n)
    firstCode(level) = code

    // Recurse
    var retval = firstPathNode(level + 1, numCellsRef.value)

    // Process other children if not shortcut
    if (retval >= level) {
      restoreState(level)
      retval = otherNode(level, numCells)
    }

    // FIX #1: Group size calculation using orbit index (like C nauty)
    // Count vertices in target cell that are in same orbit as tv1
    // This is the index of the stabilizer (number of equivalent vertices)
    val (_, tcEnd) = cellBounds(firstTc(level), level - 1)
    var index = 0
    var i = firstTc(level)
    while (i < tcEnd) {
      // Get the vertex at position i from the saved labeling
      val v = labStack(level)(i)
      // Check if this vertex is in the same orbit as tv1
      if (Orbits.sameOrbit(orbits, v, tv1)) {
        index += 1
      }
      i += 1
    }
    if (index > 0) {
      stats.multiplyGroupSize(index)
    }

    retval
  }

  /**
   * Process non-first path nodes.
   */
  def otherNode(level: Int, numCells: Int): Int = {
    // Get target cell
    val tcStart = firstTc(level)
    val (_, tcEnd) = cellBounds(tcStart, level - 1)

    var i = tcStart + 1
    while (i < tcEnd) {
      restoreState(level)
      stats.numNodes += 1

      // Reset equivalence level for this new path
      eqlevFirst = level - 1

      val tv = lab(i)
      if (orbits(tv) == tv) {  // Only process orbit representatives
        breakout(level, tcStart, tv)

        val numCellsRef = IntRef(numCells + 1)
        val code = Refinement.refine(g, lab, ptn, level, numCellsRef, active, m, n)

        // Compare with first path and track equivalence level
        if (code < firstCode(level)) {
          // Prune - less than first path
          stats.numBadLeaves += 1
        } else if (code > firstCode(level)) {
          // Prune - greater than first path
          stats.numBadLeaves += 1
        } else {
          // Same code - update equivalence tracking
          eqlevFirst = level
          // Continue search
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
      // Reached a leaf - check for automorphism or better canon
      processLeaf(level)
      return level - 1
    }

    val tcellSize = makeTargetCell(level, numCells)
    val tcStart = findCellStart(level)

    saveState(level)

    val (_, tcEnd) = cellBounds(tcStart, level - 1)
    var i = tcStart

    while (i < tcEnd) {
      if (i > tcStart) restoreState(level)

      val tv = lab(i)
      breakout(level, tcStart, tv)

      val numCellsRef = IntRef(numCells + 1)
      val code = Refinement.refine(g, lab, ptn, level, numCellsRef, active, m, n)

      // Compare with first path code and update equivalence level
      if (code == firstCode(level)) {
        if (eqlevFirst == level - 1) {
          eqlevFirst = level
        }
        val retval = processNode(level + 1, numCellsRef.value)
        if (retval < level - 1) return retval
      } else {
        // Code doesn't match first path - this isn't an automorphism path
        // but might still lead to a better canonical form
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
   * Process first leaf (terminal node).
   */
  def firstTerminal(level: Int): Unit = {
    stats.maxLevel = math.max(stats.maxLevel, level)

    // Save first labeling
    System.arraycopy(lab, 0, firstLab, 0, n)

    // Initialize canonical data
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
   */
  def processLeaf(level: Int): Unit = {
    stats.maxLevel = math.max(stats.maxLevel, level)

    // Check if this is an automorphism
    // Only check if codes matched all the way down (eqlevFirst == level)
    // This is a precondition from the C implementation
    if (eqlevFirst >= level - 1 && isAutomorphism()) {
      // Found an automorphism
      val perm = new Array[Int](n)
      computePermutation(perm)
      recordAutomorphism(perm)
    } else if (options.getCanon != 0) {
      // Check if better than current canonical
      val cmp = compareWithCanon()
      if (cmp < 0) {
        // Better canonical labeling found
        System.arraycopy(lab, 0, canonLab, 0, n)
        updateCanon(level)
        stats.canUpdates += 1
        canonLevel = level
        eqlevCanon = level
      }
    }
  }

  /**
   * Check if current labeling gives an automorphism.
   */
  def isAutomorphism(): Boolean = {
    // Compare g^lab with g^firstLab
    // This is true if lab is a relabeling that preserves adjacency

    // Build permutation from firstLab to lab
    val perm = new Array[Int](n)
    computePermutation(perm)

    // Check if perm is an automorphism
    var i = 0
    while (i < n) {
      val neighbors = g.neighbors(i)
      for (j <- neighbors) {
        if (!g.hasEdge(perm(i), perm(j))) return false
      }
      i += 1
    }
    true
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
   *
   * Returns true if this was a new generator (orbits changed), false if redundant.
   */
  def recordAutomorphism(perm: Array[Int]): Boolean = {
    // Capture old orbit count before joining
    val oldNumOrbits = stats.numOrbits
    val numOrbits = Orbits.orbjoin(orbits, perm, n)
    stats.numOrbits = numOrbits

    // Count fixed points
    var numFixed = 0
    var i = 0
    while (i < n) {
      if (perm(i) == i) numFixed += 1
      i += 1
    }

    // FIX #2: Only store generator if it actually changes orbits
    // This matches C nauty's check: if (stats->numorbits == save) return gca_canon;
    // If orbits didn't change, this automorphism is already generated by previous ones.
    val isNewGenerator = numFixed < n && numOrbits < oldNumOrbits

    if (isNewGenerator) {
      generators += Permutation.fromArray(perm)
      stats.numGenerators += 1
    }

    // Call user callback if provided (always called in C nauty)
    options.userAutomProc.foreach { callback =>
      callback(stats.numGenerators, perm, orbits, numOrbits, numFixed, n)
    }

    isNewGenerator
  }

  /**
   * Compare current labeling with canonical.
   */
  def compareWithCanon(): Int = {
    // Build inverse labeling
    val invLab = new Array[Int](n)
    var i = 0
    while (i < n) {
      invLab(lab(i)) = i
      i += 1
    }

    // Compare rows
    i = 0
    while (i < n) {
      val row = g.graphRow(lab(i))
      // Permute row by invLab
      SetOps.emptySet(workset, m)
      var pos = -1
      pos = SetOps.nextElement(row, m, pos)
      while (pos >= 0 && pos < n) {
        SetOps.addElement(workset, invLab(pos))
        pos = SetOps.nextElement(row, m, pos)
      }

      // Compare with canon row
      canong match {
        case Some(cg) =>
          val canonRow = cg.graphRow(i)
          var j = 0
          while (j < m) {
            val a = workset(j)
            val b = canonRow(j)
            if (a != b) {
              return java.lang.Long.compareUnsigned(a, b)
            }
            j += 1
          }
        case None =>
          // No canon yet, this is better
          return -1
      }
      i += 1
    }
    0
  }

  /**
   * Update the canonical graph.
   */
  def updateCanon(level: Int): Unit = {
    // Build inverse labeling
    val invLab = new Array[Int](n)
    var i = 0
    while (i < n) {
      invLab(lab(i)) = i
      i += 1
    }

    // Build canonical graph
    val canonData = new Array[Long](n * m)
    i = 0
    while (i < n) {
      val row = g.graphRow(lab(i))
      var pos = -1
      pos = SetOps.nextElement(row, m, pos)
      while (pos >= 0 && pos < n) {
        val j = invLab(pos)
        canonData(i * m + (j >> 6)) |= SetWord.bit(j & 0x3F)
        pos = SetOps.nextElement(row, m, pos)
      }
      i += 1
    }

    canong = Some(DenseGraph.fromData(canonData, n, m, g.directed))
  }

  /**
   * Break out a vertex from a cell.
   */
  def breakout(level: Int, tcStart: Int, tv: Int): Unit = {
    SetOps.emptySet(active, m)
    SetOps.addElement(active, tcStart)

    // Find tv in the cell and move it to front
    var i = tcStart
    var prev = tv

    while (lab(i) != tv) {
      val next = lab(i)
      lab(i) = prev
      prev = next
      i += 1
    }
    lab(i) = prev

    // Mark split - use level (not level - 1) per nauty convention
    ptn(tcStart) = level
  }

  /**
   * Make target cell for this level.
   */
  def makeTargetCell(level: Int, numCells: Int): Int = {
    // Find first non-singleton cell at the current level.
    // A cell ends at position i if ptn(i) <= level.
    // So we skip positions where ptn(i) <= level (singleton cells).
    var i = 0
    while (i < n && ptn(i) <= level) {
      i += 1
    }

    if (i == n) {
      // All singletons
      SetOps.emptySet(tcell, m)
      0
    } else {
      // Find cell bounds - continue while ptn(i) > level (still in same cell)
      val start = i
      while (i < n - 1 && ptn(i) > level) {
        i += 1
      }
      val cellSize = i - start + 1

      // Build target cell set
      SetOps.emptySet(tcell, m)
      var j = start
      while (j <= i) {
        SetOps.addElement(tcell, lab(j))
        j += 1
      }

      // Copy to stack
      System.arraycopy(tcell, 0, tcellStack(level), 0, m)

      cellSize
    }
  }

  /**
   * Find the starting position of the target cell.
   */
  def findCellStart(level: Int): Int = {
    // Skip singleton cells (those ending at or before current level)
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

package com.thatdot.nauty.util

import com.thatdot.nauty.graph.Graph

/**
 * Callback invoked for each automorphism found.
 */
trait AutomorphismCallback {
  /**
   * Called for each generator of the automorphism group.
   *
   * @param count     Sequence number of this automorphism (1-based)
   * @param perm      The permutation (array where perm(i) = image of vertex i)
   * @param orbits    Current orbit partition (orbits(i) = representative of i's orbit)
   * @param numOrbits Number of orbits
   * @param numFixed  Number of vertices fixed by this automorphism
   * @param n         Number of vertices
   */
  def apply(count: Int, perm: Array[Int], orbits: Array[Int],
            numOrbits: Int, numFixed: Int, n: Int): Unit
}

/**
 * Callback invoked at each level of the search tree.
 */
trait LevelCallback {
  /**
   * Called when entering/leaving a level in the search tree.
   *
   * @param lab       Current vertex labeling
   * @param ptn       Current partition
   * @param level     Current level (0 = before search, 1 = root, etc.)
   * @param orbits    Current orbit partition
   * @param stats     Current statistics
   * @param tv        Target vertex
   * @param index     Index in target cell
   * @param tcellSize Size of target cell
   * @param numCells  Number of cells in partition
   * @param childCount Number of children at this node
   * @param n         Number of vertices
   */
  def apply(lab: Array[Int], ptn: Array[Int], level: Int, orbits: Array[Int],
            stats: NautyStats, tv: Int, index: Int, tcellSize: Int,
            numCells: Int, childCount: Int, n: Int): Unit
}

/**
 * Callback invoked at each node of the search tree.
 */
trait NodeCallback {
  /**
   * Called when visiting a node in the search tree.
   */
  def apply(g: Graph[_], lab: Array[Int], ptn: Array[Int], level: Int,
            numCells: Int, tv: Int, index: Int, tcellSize: Int, n: Int): Unit
}

/**
 * Callback invoked when a potentially better canonical labeling is found.
 * Return value determines whether to accept or reject.
 */
trait CanonCallback {
  /**
   * Called when a potentially better canonical form is found.
   *
   * @param g        The graph
   * @param lab      Proposed canonical labeling
   * @param canong   Current best canonical graph
   * @param hash     Hash value for comparison
   * @param level    Search tree level
   * @param m        Number of setwords per row
   * @param n        Number of vertices
   * @return 0 to accept, 1 to reject and stop exploring, -1 to reject but continue
   */
  def apply(g: Graph[_], lab: Array[Int], canong: Graph[_],
            hash: Long, level: Int, m: Int, n: Int): Int
}

/**
 * Vertex invariant procedure for partition refinement.
 */
trait VertexInvariant {
  /**
   * Compute vertex invariants to assist partition refinement.
   *
   * @param g        The graph
   * @param lab      Current labeling
   * @param ptn      Current partition
   * @param level    Search tree level
   * @param numCells Number of cells
   * @param tv       Target vertex
   * @param invar    Array to fill with invariant values
   * @param invarArg Argument passed from options
   * @param digraph  True if directed graph
   * @param m        Number of setwords per row
   * @param n        Number of vertices
   */
  def apply(g: Graph[_], lab: Array[Int], ptn: Array[Int], level: Int,
            numCells: Int, tv: Int, invar: Array[Int], invarArg: Int,
            digraph: Boolean, m: Int, n: Int): Unit
}

/**
 * Options controlling nauty's behavior.
 *
 * @param getCanon          Whether to compute canonical labeling (0=no, 1=yes, 2=label only)
 * @param digraph           True for directed graphs (or graphs with loops)
 * @param writeAutoms       Write automorphisms to output (for debugging)
 * @param writeMarkers      Write markers during search (for debugging)
 * @param defaultPtn        Use trivial initial partition
 * @param cartesian         Use cartesian format for automorphism output
 * @param lineLength        Max line length for output
 * @param userAutomProc     Custom callback for automorphisms
 * @param userLevelProc     Custom callback for levels
 * @param userNodeProc      Custom callback for nodes
 * @param userCanonProc     Custom callback for canonical labeling
 * @param invarProc         Vertex invariant procedure
 * @param tcLevel           Max level for smart target cell selection
 * @param minInvarLevel     Min level for invariant computation
 * @param maxInvarLevel     Max level for invariant computation
 * @param invarArg          Argument passed to invariant procedure
 * @param schreier          Use Schreier-Sims for group computation
 */
case class NautyOptions(
  getCanon: Int = 0,
  digraph: Boolean = false,
  writeAutoms: Boolean = false,
  writeMarkers: Boolean = false,
  defaultPtn: Boolean = true,
  cartesian: Boolean = false,
  lineLength: Int = 78,
  userAutomProc: Option[AutomorphismCallback] = None,
  userLevelProc: Option[LevelCallback] = None,
  userNodeProc: Option[NodeCallback] = None,
  userCanonProc: Option[CanonCallback] = None,
  invarProc: Option[VertexInvariant] = None,
  tcLevel: Int = 100,
  minInvarLevel: Int = 0,
  maxInvarLevel: Int = 1,
  invarArg: Int = 0,
  schreier: Boolean = false,
  initialPartition: Option[Seq[Seq[Int]]] = None
) {
  /** Enable canonical labeling computation */
  def withCanon: NautyOptions = copy(getCanon = 1)

  /** Disable canonical labeling computation */
  def withoutCanon: NautyOptions = copy(getCanon = 0)

  /** Enable digraph mode */
  def asDigraph: NautyOptions = copy(digraph = true)

  /** Enable Schreier-Sims */
  def withSchreier: NautyOptions = copy(schreier = true)

  /**
   * Set initial partition (vertex coloring).
   * Vertices in the same cell are considered equivalent by the initial partition.
   * Example: Seq(Seq(0), Seq(1, 2, 3)) puts vertex 0 in its own cell, vertices 1,2,3 together.
   */
  def withPartition(cells: Seq[Seq[Int]]): NautyOptions =
    copy(defaultPtn = false, initialPartition = Some(cells))
}

object NautyOptions {
  /** Default options for undirected graphs */
  val defaultGraph: NautyOptions = NautyOptions()

  /** Default options for directed graphs */
  val defaultDigraph: NautyOptions = NautyOptions(
    digraph = true,
    maxInvarLevel = 999
  )

  /** Default options for sparse graphs */
  val defaultSparseGraph: NautyOptions = NautyOptions()

  /** Default options for sparse directed graphs */
  val defaultSparseDigraph: NautyOptions = NautyOptions(
    digraph = true,
    maxInvarLevel = 999
  )
}

/**
 * Options for the Traces algorithm.
 */
case class TracesOptions(
  getCanon: Boolean = false,
  digraph: Boolean = false,
  writeAutoms: Boolean = false,
  verbosity: Int = 0,
  defaultPtn: Boolean = true,
  userAutomProc: Option[AutomorphismCallback] = None,
  strategy: Int = 0,
  weightedLab: Boolean = false
) {
  def withCanon: TracesOptions = copy(getCanon = true)
  def asDigraph: TracesOptions = copy(digraph = true)
}

object TracesOptions {
  val default: TracesOptions = TracesOptions()
}
